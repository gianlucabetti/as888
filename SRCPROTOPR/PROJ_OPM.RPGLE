000100071116      //************************************************************************
000200071116      //
000300071116      // Prototipi per chiamate a procedure Proj da procedure RPG ILE.
000400071116      //
000500071116      //************************************************************************
000600071116     D Proj_AllineaCodice...
000700071205     D                 PR                  EXTPGM('XALLINEA')
000800071116     D  codice                       31A   OPTIONS(*VARSIZE)
000900071116     D  len                           5P 0 CONST
001000071116     D  tipo                          1A   CONST
001100071205
001200071205     D Proj_CodiceFiscale...
001300071205     D                 PR                  EXTPGM('XCDFIS')
001400071205     D  codiceFiscale                20A
001500071205     D  esito                         1P 0
001600071116
001700071116     D Proj_EsistenzaCommenti...
001800071116     D                 PR                  EXTPGM('ANA724R')
001900071116     D  kpjba                              LIKEDS(kpjba)
002000071116
002100071116     D Proj_EstrattoConto...
002200071116     D                 PR                  EXTPGM('NDC085R')
002300071116     D  kpjba                              LIKEDS(kpjba)
002400071116
002500071116     D Proj_GestioneCommenti...
002600071116     D                 PR                  EXTPGM('ANA725R')
002700071116     D  kpjba                              LIKEDS(kpjba)
002800071116
002900071116     D Proj_InserimentoCommento...
003000071116     D                 PR                  EXTPGM('ANA726R')
003100071116     D  kpjba                              LIKEDS(kpjba)
003200071116
003300071116     D Proj_InterrogazioneConto...
003400071116     D                 PR                  EXTPGM('ANA399R')
003500071116     D  kpjba                              LIKEDS(kpjba)
003600071116
003700071116     D Proj_Numeratore...
003800071205     D                 PR                  EXTPGM('XNMR')
003900071116     D  xnmRic                        1A   CONST
004000071116     D  xnmSoc                        3A   CONST
004100071116     D  xnmUni                        8A   CONST
004200071116     D  xnmCtb                        2A   CONST
004300071116     D  xnmKey                        8A   CONST
004400071116     D  xnmSer                        4A   CONST
004500071116     D  xnmCmt                        1A   CONST
004600071116     D  xnmDat                         D   CONST
004700071116     D  xnmIva                        1A   CONST
004800071116     D  xnmReg                        1A   CONST
004900071116     D  xnmErr                        1A
005000071116     D  xnmNum                        9P 0
005100071116     D  xnmAut                        1A
005200071116     D  xnmDag                         D   CONST OPTIONS(*NOPASS)
005300071116
005400071116     D Proj_Opzioni...
005500071205     D                 PR                  EXTPGM('X04OPMSG')
005600071116     D  $tx                          50A   DIM(50)
005700071116     D  $fl                           1A   DIM(50)
005800071116     D  cntOpz                        3P 0
005900071116     D  maxOpz                        3P 0
006000071116     D  opz1                         76A
006100071116     D  opz2                         76A
006200071116     D  ff23                          1A
006300071116     D  $l01                          3P 0 OPTIONS(*NOPASS)
006400071116     D  $l02                          3P 0 OPTIONS(*NOPASS)
006500071205
006600071205     D Proj_PartitaIVA...
006700071205     D                 PR                  EXTPGM('XPARIVA')
006800071205     D  partitaIVA                   20A
006900071205     D  esito                         1P 0
007000071205     D  societa                       3A
007100071116
007200071116     D Proj_RicercaCliente...
007300071116     D                 PR                  EXTPGM('ANA300R')
007400071116     D  kpjba                              LIKEDS(kpjba)
007500071116
007600071116     D Proj_RicercaCondizionePagamento...
007700071116     D                 PR                  EXTPGM('ANA024R')
007800071116     D  kpjba                              LIKEDS(kpjba)
007900071116
008000100804     D Proj_RicercaFornitore...
008100100804     D                 PR                  EXTPGM('ANA400R')
008200100804     D  kpjba                              LIKEDS(kpjba)
008300100804
008400071116     D Proj_RicercaUnita...
008500071116     D                 PR                  EXTPGM('ANA009R')
008600071116     D  kpjba                              LIKEDS(kpjba)
008700071116
008800071116     D Proj_Soggetto...
008900071205     D                 PR                  EXTPGM('NDDVASOG')
009000071116     D  richiesta                     1A
009100071116     D  societa                       3A
009200071116     D  unita                         8A
009300071116     D  strutturaDati                10A
009400071116     D  dataRiferimento...
009500071116     D                                 D
009600071116     D  lunghezzaDati...
009700071116     D                                9B 0
009800071116     D  sottoNatura...
009900071116     D                                1A
010000071116     D  codice...
010100071116     D                                8A
010200071116     D  lineaVendita...
010300071116     D                                3A
010400071116     D  filiale...
010500071116     D                                3P 0
010600071116     D  soggetto...
010700071116     D                                8A   OPTIONS(*OMIT)
010800071116     D  tipoIndirizzo...
010900071116     D                                2A
011000071116     D  codiceIndirizzo...
011100071116     D                                3A
011200071116     D  errore...
011300071116     D                                1A
011400071116     D  output...
011500071116     D                             4000A   OPTIONS(*VARSIZE)
011600071116
011700071116     D Proj_TastiFunzionali...
011800071205     D                 PR                  EXTPGM('XKEYMSG')
011900071116     D  $tx                          50A   DIM(24)
012000071116     D  $fc                           1A   DIM(24)
012100071116     D  $fr                           1A   DIM(24)
012200071116     D  $ulk                          3P 0
012300071116     D  $key1                        79A
012400071116     D  $key2                        79A
012500071116     D  $l01                          3P 0 OPTIONS(*NOPASS)
012600071116     D  $l02                          3P 0 OPTIONS(*NOPASS)
012700071116
012800071116     D Proj_Testo...
012900071116     D                 PR                  EXTPGM('ANA721R')
013000071116     D  kpjba                              LIKEDS(kpjba)
013100071116
