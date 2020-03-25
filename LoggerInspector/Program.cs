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
                var filePath = Console.ReadLine();
                while (!string.IsNullOrEmpty(filePath) && !File.Exists(filePath))
                {
                    logger.LogWarning("file path '{filePath}' not found, try again", filePath);
                    filePath = Console.ReadLine();
                }

                if (string.IsNullOrEmpty(filePath))
                {
                    logger.LogInformation("exiting...");
                    return;
                }

                await fileInspector.Inspect(filePath);
            }
        }
    }
}