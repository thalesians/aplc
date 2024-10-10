#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cerrno>

#include <unistd.h>

#include "node.h"
#include "parser.h"
#include "codegen.h"

extern ExpressionList* expression_list;
extern int yyparse();
extern int yylex();
extern FILE *yyin;

void usage(char *arg)
{
    std::cerr << "Usage: " << arg << " [-t] <file>" << std::endl;
    std::exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
    std::cout << "aplc v0.0.1" << std::endl;

    int opt;
    bool output_tokens = false;

    while ((opt = getopt(argc, argv, "t")) != -1) {
        switch (opt) {
        case 't':
            output_tokens = true;
            break;
        default:
            usage(argv[0]);
        }
    }
    if (argc - optind != 1) { usage(argv[0]); }

    if (std::strcmp(argv[optind], "-")) {
        if (errno = 0, !(yyin = std::fopen(argv[optind], "r"))) {
            perror(argv[optind]);
            std::exit(EXIT_FAILURE);
        }
    }

    if (output_tokens) {
        int next_token;
        while ((next_token = yylex())) {
            switch (next_token) {
              case TINTEGER:
                std::cout << "Integer " << *yylval.string << std::endl; break;
              case TSTRING:
                std::cout << "String " << *yylval.string << std::endl; break;
              case TIDENTIFIER:
                std::cout << "Identifier " << *yylval.string << std::endl; break;
              default:
                std::cout << "Token " << next_token << std::endl; break;
            }
        }
    } else {
        std::cout << "Parsing..." << std::endl;
        yyparse();
        std::cout << "Done parsing." << std::endl;
        if (expression_list) {
            std::cout << "Got an input expression list..." << std::endl;
            std::stringstream ss;
            for (size_t i = 0; i < expression_list->size(); ++i) {
                (*expression_list)[i]->print(ss);
            }
            std::cout << "Here is what I've read:" << std::endl;
            std::cerr << ss.str();
            std::cout << "(done)" << std::endl;
            
            std::unique_ptr<LLVMContext> TheContext;
            std::unique_ptr<Module> TheModule;
            std::unique_ptr<IRBuilder<>> Builder;

            TheContext = std::make_unique<LLVMContext>();
            TheModule = std::make_unique<Module>(ss.str(), *TheContext);
            // Create a new builder for the module.
            Builder = std::make_unique<IRBuilder<>>(*TheContext);
            
            // ExpressionList * exprs = new ExpressionList();
            generate_code(expression_list, *TheContext, *Builder, *TheModule);
        }
    }

    if (errno = 0, std::fclose(yyin)) {
        perror(argv[optind]);
    }

    return 0;
}
