Sistema de Inventário em Haskell

Atividade Avaliativa – RA2

Disciplina: Programação Lógica e Funcional
Professor: Frank Coelho de Alcantara
PUCPR

Grupo - Henry Mendes(HenryMendesr), Kaue Alaniz(KaueAlaniz Kaue), Matheus Bernardi(maththe Matheus Bernardi), Rafael Maluf(RafaMaluf Rafael Maluf)

Como compilar e executar o programa no Online GDB:

Crie/copie todos os arquivos .hs no GDB, selecione a linguagem Haskell e clique em run.
https://www.onlinegdb.com/#

Exemplo de uso: 
Adicionar produto

após rodar no terminal coloque: -"add" -"Id do item" -"Nome do item" -"quantidade" -"categoria"

Visão Geral

O projeto implementa um sistema de inventário em Haskell, executado no terminal.

Utilizamos:

Funções puras

Funções impuras (IO)

ADTs

Serialização e desserialização

Persistência de estado

Análise de logs

Organização do Código — Por que dividimos em arquivos?

Nós escolhemos separar o sistema em múltiplos módulos por três motivos principais:

1. Clareza e leitura do código

Quando tudo fica dentro de um único arquivo gigante, a lógica começa a se misturar.
Separando em módulos, cada parte fica com uma responsabilidade.

Isso facilita:

leitura

manutenção

entendimento geral

2. Aproximação do que é feito em sistemas reais

Em sistemas maiores, é inviável ter um arquivo único.

Com isso, o grupo consegue:

modificar o projeto com facilidade

testar módulos individualmente

modificar partes sem quebrar o resto

3. Separação explícita entre “puro” e “impuro”

Funções puras:

são simples

recebem valores e retornam valores

Funções impuras lidam com:

arquivos

terminal

IO

tempo real

exceções

Misturar as duas coisas no mesmo arquivo ficaria confuso e atrapalharia a compreensão do projeto.


Estrutura dos Arquivos (Geral)

Logica.hs – Funções 100% puras.
Lógica de negócio isolada, fácil de testar e compreender.

Relatorios.hs – Funções puras.
Relatórios funcionam apenas com listas e filtros.

Persistencia.hs – IO puro.
Tudo relacionado a arquivos fica aqui.

Main.hs – IO + loop.
Parte interativa, comandos, prints, leitura e gravação.

Tipos.hs – Modelos e ADTs.
Base da aplicação.

Estrutura do Repositório
Inventario-RA2/

 Main.hs
 Tipos.hs
 Logica.hs
 Persistencia.hs
 Relatorios.hs

 Inventario.dat      # Gerado automaticamente
 Auditoria.log       # Gerado automaticamente

Explicação dos Módulos

Tipos.hs
Modelos e ADTs essenciais. Aqui nasce tudo que o resto do programa usa.

Logica.hs
Parte de cálculo do sistema. Funções puras, sem IO.

Persistencia.hs
Tudo relacionado a armazenamento: .dat, .log e leitura.

Relatorios.hs
Análise 100% funcional: filtros, listas e agrupamentos.

Main.hs
O programa como o usuário enxerga: menu, input, prints e loop.

Cenários de Teste

Cenário 1
https://youtu.be/uWuQ65-yDsc

Cenário 2 e 3
https://youtu.be/rIRFNr1a23g

Conclusão

Decidimos dividir o projeto em múltiplos arquivos e módulos porque:

deixa mais organizado

facilita entendimento entre os membros do grupo e o professor

separa pureza de IO com clareza

evita mistura de responsabilidades

aproxima o trabalho da vida real em projetos funcionais

Essa abordagem tornou o desenvolvimento mais simples, mais didático e mais alinhado ao objetivo da disciplina: dominar programação funcional aplicada, e não apenas “fazer rodar”.
