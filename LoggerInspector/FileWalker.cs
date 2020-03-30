using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.Logging;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace LoggerInspector
{
    public class FileWalker : CSharpSyntaxRewriter
    {
        private bool _removedOldLogger;
        private int _counter;

        private readonly ILogger<FileWalker> _logger;

        public FileWalker(ILogger<FileWalker> logger, bool visitIntoStructuredTrivia = false) : base(visitIntoStructuredTrivia)
        {
            _logger = logger;
        }

        public SemanticModel SemanticModel { get; set; }

        public bool IsUpdated => _counter > 0 || _removedOldLogger;

        public override SyntaxNode? VisitCompilationUnit(CompilationUnitSyntax node)
        {
            var retVal = (CompilationUnitSyntax) base.VisitCompilationUnit(node);

            if (!IsUpdated)
            {
                _logger.LogWarning("no updates found");
                return retVal;
            }

            const string name = "Microsoft.Extensions.Logging";
            if (_counter > 0 && retVal.Usings.All(x => x.Name.ToString() != name))
            {
                _logger.LogInformation("adding new using {name}", name);

                var newUsing = UsingDirective(
                    QualifiedName(
                        QualifiedName(
                            IdentifierName("Microsoft"),
                            IdentifierName("Extensions")
                        ),
                        IdentifierName("Logging")
                    )
                ).WithUsingKeyword(
                    Token(
                        TriviaList(),
                        SyntaxKind.UsingKeyword,
                        TriviaList(
                            Space
                        )
                    )
                ).WithSemicolonToken(
                    Token(
                        TriviaList(),
                        SyntaxKind.SemicolonToken,
                        TriviaList(
                            LineFeed
                        )
                    )
                );
                retVal = retVal.AddUsings(newUsing);
            }

            return retVal;
        }

        public override SyntaxNode? VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            var retVal = (ClassDeclarationSyntax) base.VisitClassDeclaration(node);

            var fieldDeclarationSyntaxes = retVal.Members.OfType<FieldDeclarationSyntax>().ToList();
            var exists = fieldDeclarationSyntaxes.Any(x => x.Declaration.Variables.Any(y => y.ToString() == "_logger"));
            if (!exists)
            {
                var className = node.Identifier.Text;
                if (!HasLoggerUsage(node))
                {
                    _logger.LogWarning("skipping class '{className}' for adding logger field", className);
                    return retVal;
                }

                _logger.LogInformation("adding logger field");

                var loggerField = FieldDeclaration(
                            VariableDeclaration(
                                    GenericName(
                                            Identifier("ILogger")
                                        )
                                        .WithTypeArgumentList(
                                            TypeArgumentList(
                                                SingletonSeparatedList<TypeSyntax>(
                                                    IdentifierName(node.Identifier.Text)
                                                )
                                            )
                                        )
                                )
                                .WithVariables(
                                    SingletonSeparatedList<VariableDeclaratorSyntax>(
                                        VariableDeclarator(
                                            Identifier("_logger")
                                        )
                                    )
                                )
                        )
                        .WithModifiers(
                            TokenList(
                                new[]
                                {
                                    Token(SyntaxKind.PrivateKeyword),
                                    Token(SyntaxKind.ReadOnlyKeyword)
                                }
                            )
                        )
                        .NormalizeWhitespace()
                        .WithSemicolonToken(
                            Token(
                                TriviaList(),
                                SyntaxKind.SemicolonToken,
                                TriviaList(
                                    LineFeed
                                )
                            )
                        )
                        .WithLeadingTrivia(node.Members[0].GetLeadingTrivia())
                    ;

                var index = retVal.Members.IndexOf(fieldDeclarationSyntaxes.LastOrDefault()) + 1;
                retVal = retVal.WithMembers(retVal.Members.Insert(index, loggerField));
            }

            return retVal;
        }

        public override SyntaxNode? VisitUsingDirective(UsingDirectiveSyntax node)
        {
            var name = node.Name.ToString();
            if (name == "Castle.Core.Logging")
            {
                _logger.LogInformation("removing using {name}", name);
                _removedOldLogger = true;
                return null;
            }

            return base.VisitUsingDirective(node);
        }

        public override SyntaxNode? VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var propType = node.Type.ToString();
            var propName = node.Identifier.ValueText;
            if (propType == "ILogger" && propName == "Logger")
            {
                _logger.LogInformation("removing old logger property");
                _removedOldLogger = true;
                return null;
            }

            return base.VisitPropertyDeclaration(node);
        }

        public override SyntaxNode? VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            if (node.ParameterList.Parameters.All(x => x.Identifier.Text != "logger"))
            {
                if (!HasLoggerUsage(node.Parent))
                {
                    var className = node.Identifier.ValueText;
                    _logger.LogWarning("skipping class '{className}' for adding new logger constructor parameter", className);
                    return base.VisitConstructorDeclaration(node);
                }

                _logger.LogInformation("adding new logger constructor parameter");

                var loggerParameter = Parameter(Identifier(TriviaList(Space), "logger", TriviaList()))
                    .WithType(
                        GenericName(Identifier(TriviaList(Space), "ILogger", TriviaList()))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(IdentifierName(node.Identifier.Text))
                                )
                            )
                    );
                node = node.AddParameterListParameters(loggerParameter);

                if (node.Body == null)
                {
                    node = node.WithBody(Block());
                }

                _logger.LogInformation("adding new logger assign statement");
                var loggerAssignExpression = ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            IdentifierName("_logger"),
                            IdentifierName("logger")
                        )
                    )
                    .NormalizeWhitespace()
                    .WithLeadingTrivia(node.Body.Statements.FirstOrDefault()?.GetLeadingTrivia() ?? TriviaList(Whitespace("            ")))
                    .WithSemicolonToken(
                        Token(
                            TriviaList(),
                            SyntaxKind.SemicolonToken,
                            TriviaList(
                                LineFeed
                            )
                        )
                    );

                node = node.WithBody(node.Body.AddStatements(loggerAssignExpression));
            }

            return base.VisitConstructorDeclaration(node);
        }

        public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            if (node.GetFirstToken().Text == "Logger")
            {
                var isConstructorDeclaration = node.AncestorsAndSelf().Any(x => x.Kind() == SyntaxKind.ConstructorDeclaration);
                if (isConstructorDeclaration)
                {
                    _logger.LogInformation("removing old logger assignment expression in constructor");
                    _removedOldLogger = true;
                    return null;
                }

                var isMethodOrPropertyDeclaration = node.AncestorsAndSelf().Any(x =>
                {
                    var kind = x.Kind();
                    return kind == SyntaxKind.MethodDeclaration || kind == SyntaxKind.PropertyDeclaration;
                });
                if (isMethodOrPropertyDeclaration)
                {
                    if (node.Expression.Kind() == SyntaxKind.InvocationExpression)
                    {
                        _logger.LogInformation("┌────────────{counter}────────────┐", ++_counter);
                        _logger.LogInformation("refactoring: {expression}", node.ToString());


                        // method
                        var invocationExpressionSyntax = (InvocationExpressionSyntax) node.Expression;
                        var methodName = invocationExpressionSyntax.Expression.GetLastToken().ToString();
                        methodName = GetLoggerFunctionName(methodName);

                        // args
                        var argumentList = invocationExpressionSyntax.ArgumentList;
                        var newArgList = ArgumentList(SeparatedList<ArgumentSyntax>());
                        var argumentsToAdd = new List<ArgumentSyntax>();

                        // find exception parameter
                        var exceptionArgumentSyntax = argumentList.Arguments.FirstOrDefault(x => IsException(SemanticModel.GetTypeInfo(x.Expression).Type));
                        if (exceptionArgumentSyntax != null)
                        {
                            _logger.LogInformation("found exception param: {name}", exceptionArgumentSyntax.ToString());
                            newArgList = newArgList.AddArguments(exceptionArgumentSyntax);
                        }

                        // find message parameter
                        var messageArgumentSyntax = argumentList.Arguments
                            .First(x =>
                            {
                                var typeName = SemanticModel.GetTypeInfo(x.Expression).Type.Name;
                                var kind = x.Expression.Kind();
                                return typeName == nameof(String) &&
                                       (kind == SyntaxKind.StringLiteralExpression ||
                                        kind == SyntaxKind.InterpolatedStringExpression ||
                                        kind == SyntaxKind.AddExpression ||
                                        kind == SyntaxKind.IdentifierName ||
                                        kind == SyntaxKind.SimpleMemberAccessExpression ||
                                        kind == SyntaxKind.InvocationExpression);
                            });
                        var messageArgumentExpressionKind = messageArgumentSyntax.Expression.Kind();

                        if (messageArgumentExpressionKind == SyntaxKind.StringLiteralExpression)
                        {
                            var messageArgumentIndex = argumentList.Arguments.IndexOf(messageArgumentSyntax);
                            var str = messageArgumentSyntax.Expression.GetFirstToken().ValueText;
                            var index = 0;

                            var indexStr = $"{{{index++}}}";
                            var foundIndex = str.IndexOf(indexStr);

                            while (foundIndex != -1)
                            {
                                // find argument to replace index
                                var argIndex = messageArgumentIndex + index;
                                if (argIndex < argumentList.Arguments.Count)
                                {
                                    var arg = argumentList.Arguments[argIndex];
                                    argumentsToAdd.Add(arg);
                                    var propertyStr = "{" + Regex.Replace(arg.Expression.ToString(), "[^A-Za-z0-9]", "") + "}";

                                    _logger.LogDebug("replacing {indexStr} to {propertyStr}", indexStr, propertyStr);
                                    str = str.Replace(indexStr, propertyStr);
                                }

                                // next
                                indexStr = $"{{{index++}}}";
                                foundIndex = str.IndexOf(indexStr);
                            }

                            _logger.LogInformation("adding message param: {message}", str);
                            var messageArgument = LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(str));
                            newArgList = newArgList.AddArguments(Argument(messageArgument));

                            foreach (var argumentSyntax in argumentsToAdd)
                            {
                                _logger.LogInformation("adding other param: {param}", argumentSyntax.Expression.ToString());
                                newArgList = newArgList.AddArguments(argumentSyntax);
                            }
                        }

                        if (messageArgumentExpressionKind == SyntaxKind.InterpolatedStringExpression)
                        {
                            var syntax = (InterpolatedStringExpressionSyntax) messageArgumentSyntax.Expression;
                            var str = string.Concat(syntax.Contents.Select(x => x.ToString()));

                            // fetch param from interpolation string
                            var interpolationSyntaxes = syntax.Contents.OfType<InterpolationSyntax>();
                            foreach (var interpolationSyntax in interpolationSyntaxes)
                            {
                                var name = interpolationSyntax.Expression.ToString();
                                var arg = Argument(IdentifierName(name));

                                var propertyStr = Regex.Replace(name, "[^A-Za-z0-9]", "");
                                if (name != propertyStr)
                                {
                                    name = $"{{{name}}}";
                                    propertyStr = $"{{{propertyStr}}}";
                                    _logger.LogDebug("replacing {interpolationStr} to {propertyStr}", name, propertyStr);
                                    str = str.Replace(name, propertyStr);
                                }

                                argumentsToAdd.Add(arg);
                            }

                            _logger.LogInformation("adding message param: {message}", str);
                            newArgList = newArgList.AddArguments(Argument(LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(str))));

                            foreach (var argumentSyntax in argumentsToAdd)
                            {
                                _logger.LogInformation("adding other param: {param}", argumentSyntax.Expression.ToString());
                                newArgList = newArgList.AddArguments(argumentSyntax);
                            }
                        }

                        if (messageArgumentExpressionKind == SyntaxKind.AddExpression)
                        {
                            var str = messageArgumentSyntax.Expression.ToString();
                            _logger.LogWarning("adding message param with string concat: {message}", str);
                            newArgList = newArgList.AddArguments(messageArgumentSyntax);
                        }

                        if (messageArgumentExpressionKind == SyntaxKind.IdentifierName ||
                            messageArgumentExpressionKind == SyntaxKind.SimpleMemberAccessExpression ||
                            messageArgumentExpressionKind == SyntaxKind.InvocationExpression)
                        {
                            var str = messageArgumentSyntax.Expression.ToString();
                            _logger.LogInformation("adding message param: {message}", str);
                            newArgList = newArgList.AddArguments(messageArgumentSyntax);
                        }

                        var expression = InvocationExpression(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("_logger"),
                                        IdentifierName(methodName)
                                    )
                                )
                                .WithArgumentList(
                                    newArgList.NormalizeWhitespace()
                                )
                                .WithLeadingTrivia(node.Expression.GetLeadingTrivia())
                                .WithTrailingTrivia(node.Expression.GetTrailingTrivia())
                            ;

                        // output skipped arguments
                        var skippedArguments = argumentList.Arguments
                            .Except(new[] {exceptionArgumentSyntax, messageArgumentSyntax})
                            .Except(argumentsToAdd)
                            .ToList();
                        foreach (var skippedArgument in skippedArguments)
                        {
                            _logger.LogWarning("skipped argument: {argument}", skippedArgument.ToString());
                        }

                        _logger.LogInformation("refactored: {statement}", expression.ToString());
                        _logger.LogInformation("└────────────{counterText}────────────┘", new string('─', (int) Math.Floor(Math.Log10(_counter) + 1)));

                        return node.WithExpression(expression);
                    }
                }
            }

            return base.VisitExpressionStatement(node);
        }

        private string GetLoggerFunctionName(string orgName)
        {
            if (orgName.Contains("Debug")) return "LogDebug";
            if (orgName.Contains("Info")) return "LogInformation";
            if (orgName.Contains("Warn")) return "LogWarning";
            if (orgName.Contains("Error")) return "LogError";
            if (orgName.Contains("Fatal")) return "LogCritical";

            return null;
        }

        private bool IsException(ITypeSymbol symbol)
        {
            if (symbol == null) return false;
            if (symbol.Name.EndsWith(nameof(Exception))) return true;
            return IsException(symbol.BaseType);
        }

        private bool HasLoggerUsage(SyntaxNode node)
        {
            var retVal = node.DescendantNodes().OfType<InvocationExpressionSyntax>()
                .Any(x =>
                    x.GetFirstToken().ValueText == "Logger" &&
                    x.Ancestors().Any(y => y.Kind() == SyntaxKind.MethodDeclaration || y.Kind() == SyntaxKind.PropertyDeclaration)
                );
            return retVal;
        }
    }
}