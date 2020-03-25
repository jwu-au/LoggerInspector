using System;
using System.IO;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Serilog;
using ILogger = Microsoft.Extensions.Logging.ILogger;

namespace LoggerInspector
{
    class Program
    {
        static async Task Main(string[] args)
        {
            // logging
            var logger = new LoggerConfiguration()
                .WriteTo.Debug()
                .WriteTo.Console()
                .CreateLogger();

            // services
            var services = new ServiceCollection();

            services.AddLogging(builder => builder.AddSerilog(logger));

            services.AddSingleton<FileInspector>();
            services.AddTransient<FileWalker>();

            await using var serviceProvider = services.BuildServiceProvider();

            // run
            var mainLogger = serviceProvider.GetRequiredService<ILogger<Program>>();
            var fileInspector = serviceProvider.GetRequiredService<FileInspector>();
            await Run(mainLogger, fileInspector);
        }

        static async Task Run(ILogger logger, FileInspector fileInspector)
        {
            while (true)
            {
                logger.LogInformation("input file path or [Enter] to exit");
                var path = Console.ReadLine();

                // exit
                if (string.IsNullOrEmpty(path))
                {
                    logger.LogInformation("exiting...");
                    return;
                }

                // check files
                var isDir = Directory.Exists(path);
                while (!string.IsNullOrEmpty(path) && !isDir && !File.Exists(path))
                {
                    logger.LogWarning("path '{path}' not found, try again", path);
                    path = Console.ReadLine();
                    isDir = Directory.Exists(path);
                }

                // collect files
                var paths = new[] {path};
                if (isDir)
                {
                    paths = Directory.GetFiles(path, "*.cs", SearchOption.AllDirectories);
                    if (paths.Length > 0)
                    {
                        logger.LogInformation("found {count} cs file(s) in '{path}'", paths.Length, path);
                    }
                    else
                    {
                        logger.LogWarning("found no cs file in '{path}'", path);
                        continue;
                    }
                }

                // inspect files
                foreach (var filePath in paths)
                {
                    await fileInspector.Inspect(filePath);
                }
            }
        }
    }
}