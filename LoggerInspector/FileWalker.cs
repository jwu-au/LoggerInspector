using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.Logging;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace LoggerInspector
{
    public class FileWalker : CSharpSyntaxRewriter
    {
        private readonly ILogger<FileWalker> _logger;

        public FileWalker(ILogger<FileWalker> logger, bool visitIntoStructuredTrivia = false) : base(visitIntoStructuredTrivia)
        {
            _logger = logger;
        }

        public SemanticModel SemanticModel { get; set; }

        public override SyntaxNode? VisitCompilationUnit(CompilationUnitSyntax node)
        {
            var retVal = (CompilationUnitSyntax) base.VisitCompilationUnit(node);

            const string name = "Microsoft.Extensions.Logging";
            if (retVal.Usings.All(x => x.Name.ToString() != name))
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
                return null;
            }

            return base.VisitUsingDirective(node);
        }

        public override SyntaxNode? VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var type = node.Type.ToString();
            if (type == "ILogger")
            {
                _logger.LogInformation("removing old logger property");
                return null;
            }

            return base.VisitPropertyDeclaration(node);
        }

        public override SyntaxNode? VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            if (node.ParameterList.Parameters.All(x => x.Identifier.Text != "logger"))
            {
                _logger.LogInformation("updating new logger constructor parameter");

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
                    return null;
                }

                var isMethodDeclaration = node.AncestorsAndSelf().Any(x => x.Kind() == SyntaxKind.MethodDeclaration);
                if (isMethodDeclaration)
                {
                    if (node.Expression.Kind() == SyntaxKind.InvocationExpression)
                    {
                        _logger.LogInformation("┌─────────────────────────┐");
                        _logger.LogInformation("refactoring: {expression}", node.ToString());


                        // method
                        var invocationExpressionSyntax = (InvocationExpressionSyntax) node.Expression;
                        var methodName = invocationExpressionSyntax.Expression.GetLastToken().ToString();
                        methodName = GetLoggerFunctionName(methodName);

                        // args
                        var argumentList = invocationExpressionSyntax.ArgumentList;
                        var newArgList = ArgumentList(SeparatedList<ArgumentSyntax>());

                        // find exception parameter
                        var exceptionArgumentSyntax = argumentList.Arguments.FirstOrDefault(x => IsException(SemanticModel.GetTypeInfo(x.Expression).Type));
                        if (exceptionArgumentSyntax != null)
                        {
                            _logger.LogInformation("found exception param: {name}", exceptionArgumentSyntax.ToString());
                            newArgList = newArgList.AddArguments(exceptionArgumentSyntax);
                        }

                        // find message parameter
                        var messageArgumentSyntax = argumentList.Arguments.First(x => x.Expression.Kind() == SyntaxKind.StringLiteralExpression || x.Expression.Kind() == SyntaxKind.InterpolatedStringExpression);

                        if (messageArgumentSyntax.Expression.Kind() == SyntaxKind.StringLiteralExpression)
                        {
                            var messageArgumentIndex = argumentList.Arguments.IndexOf(messageArgumentSyntax);
                            var str = messageArgumentSyntax.Expression.GetFirstToken().ValueText;
                            var index = 0;

                            var indexStr = $"{{{index++}}}";
                            var foundIndex = str.IndexOf(indexStr);
                            var argumentsToAdd = new List<ArgumentSyntax>();
                            while (foundIndex != -1)
                            {
                                // find argument to replace index
                                var argIndex = messageArgumentIndex + index;
                                if (argIndex < argumentList.Arguments.Count)
                                {
                                    var arg = argumentList.Arguments[argIndex];
                                    argumentsToAdd.Add(arg);
                                    var propertyStr = "{" + arg.Expression.ToString().Replace(".", "") + "}";

                                    _logger.LogInformation("replacing {indexStr} to {propertyStr}", indexStr, propertyStr);
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

                        if (messageArgumentSyntax.Expression.Kind() == SyntaxKind.InterpolatedStringExpression)
                        {
                            var syntax = (InterpolatedStringExpressionSyntax) messageArgumentSyntax.Expression;
                            var str = string.Concat(syntax.Contents.Select(x => x.ToString()));

                            // fetch param from interpolation string
                            var argumentsToAdd = new List<ArgumentSyntax>();
                            var interpolationSyntaxes = syntax.Contents.OfType<InterpolationSyntax>();
                            foreach (var interpolationSyntax in interpolationSyntaxes)
                            {
                                var name = interpolationSyntax.Expression.ToString();
                                var arg = Argument(IdentifierName(name));

                                var propertyStr = name.Replace(".", "");
                                if (name != propertyStr)
                                {
                                    _logger.LogInformation("replacing {interpolationStr} to {propertyStr}", name, propertyStr);
                                    str = str.Replace("{" + name + "}", "{" + propertyStr + "}");
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

                        _logger.LogInformation("refactored: {statement}", expression.ToString());
                        _logger.LogInformation("└─────────────────────────┘");

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
    }
}