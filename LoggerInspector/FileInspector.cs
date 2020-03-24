using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.Logging;

namespace LoggerInspector
{
    public class FileInspector
    {
        private readonly ILogger<FileInspector> _logger;
        private readonly FileWalker _walker;

        public FileInspector(ILogger<FileInspector> logger, FileWalker walker)
        {
            _logger = logger;
            _walker = walker;
        }

        public async Task Inspect(string filePath)
        {
            _logger.LogInformation("analyzing {filePath}", filePath);

            // build syntax tree
            var code = File.ReadAllText(filePath);
            SyntaxTree tree = CSharpSyntaxTree.ParseText(code);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            // search bin folder
            var searchPath = filePath;
            var lastIndexOfSrc = searchPath.LastIndexOf("src");
            while (lastIndexOfSrc >= 0)
            {
                var testPath = searchPath.Substring(0, lastIndexOfSrc);
                testPath = Path.Combine(testPath, "src\\Drachma.Web\\bin");
                if (Directory.Exists(testPath))
                {
                    searchPath = testPath;
                    _logger.LogInformation("found drachma bin folder {searchPath}", searchPath);
                    break;
                }

                // next
                searchPath = testPath;
                lastIndexOfSrc = searchPath.LastIndexOf("src");
            }

            if (lastIndexOfSrc == -1)
            {
                _logger.LogError("drachma bin folder not found for file path", filePath);
                return;
            }

            // symbol
            var refs = Directory.GetFiles(searchPath, "*.dll");
            _logger.LogInformation("found {count} dll(s) for symbols", refs.Length);
            var metas = refs.Select(x => MetadataReference.CreateFromFile(x)).Union(new[] {MetadataReference.CreateFromFile(typeof(object).Assembly.Location)});
            var compilation = CSharpCompilation.Create("Logger")
                .AddReferences(metas)
                .AddSyntaxTrees(tree);
            var semanticModel = compilation.GetSemanticModel(tree);
            _walker.SemanticModel = semanticModel;

            // visiting tree
            var newRoot = _walker.Visit(root);

            // write to file
            await using var writer = new StreamWriter(filePath, false, Encoding.UTF8);
            newRoot.WriteTo(writer);
            _logger.LogInformation("wrote to file {filePath}", filePath);
            _logger.LogInformation("**********done**********");
        }
    }
}