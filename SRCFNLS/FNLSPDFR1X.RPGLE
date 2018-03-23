000100110617      *---------------------------------------------------------------*
000200130318      * Trattamento DDT PDF pre-stampa - check file                   *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600130318     h bnddir('QC2LE')
000700110617      *---------------------------------------------------------------*
000800130318
000900130318     D*--------------------------------------------------------------------
001000130318     D* Apre una directory
001100130318     D*
001200130318     D* opendir(string af a dirname) ==> return memory *address of the directory
001300130318     D*--------------------------------------------------------------------
001400130318     D opendir         PR              *   EXTPROC('opendir')
001500130318     D                                 *   VALUE
001600130318     D*--------------------------------------------------------------------
001700130318
001800130318     D*--------------------------------------------------------------------
001900130318     D* Chiude una directory
002000130318     D*
002100130318     D* closedir(string af a dirname) ==> return a value of operation's result
002200130318     D*--------------------------------------------------------------------
002300130318     D closedir        PR            10I 0 EXTPROC('closedir')
002400130318     D                                 *   VALUE
002500130318     D*--------------------------------------------------------------------
002600130318
002700130318     D*--------------------------------------------------------------------
002800130318     D* Legge una directory
002900130318     D*
003000130318     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
003100130318     D*--------------------------------------------------------------------
003200130318     D readdir         PR              *   EXTPROC('readdir')
003300130318     D                                 *   VALUE
003400130318     D*--------------------------------------------------------------------
003500130321
003600130321     D*--------------------------------------------------------------------
003700130321     D* Compone e Verifica Regular Expression
003800130321     D*
003900130321     D*--------------------------------------------------------------------
004000130321     D regcomp         PR            10I 0 EXTPROC('regcomp')
004100130321     D   preg                          *   VALUE
004200130321     D   pattern                       *   VALUE
004300130321     D   cflags                      10I 0 VALUE
004400130321     D*
004500130321     D regexec         PR            10I 0 EXTPROC('regexec')
004600130321     D   preg                          *   VALUE
004700130321     D   string                        *   VALUE
004800130321     D   nmatch                      10U 0 VALUE
004900130321     D   pmatch                        *   VALUE
005000130321     D   eflags                      10I 0 VALUE
005100130321     D*
005200130321     D regfree         PR                  EXTPROC('regfree')
005300130321     D   preg                          *   VALUE
005400130321     D*
005500130321     D regex_t         DS                  align
005600130321     D  re_nsub                      10I 0
005700130321     D  re_comp                        *
005800130321     D  re_cflags                    10I 0
005900130321     D  re_erroff                    10I 0
006000130321     D  re_len                       10I 0
006100130321     D  re_ucoll                     10I 0 DIM(2)
006200130321     D  re_lsub                        *   DIM(9)
006300130321     D  re_esub                        *   DIM(9)
006400130321     D  re_map                      256A
006500130321     D  re_shift                      5I 0
006600130321     D  re_dbcs                       5I 0
006700130321     D*
006800130321     D regmatch_t      DS                  occurs(2) align
006900130321     D  rm_so                        10I 0
007000130321     D  rm_ss                         5I 0
007100130321     D  rm_eo                        10I 0
007200130321     D  rm_es                         5I 0
007300130321     D*--------------------------------------------------------------------
007400130321
007500130321     D*-------------------
007600130321     D* COSTANTI
007700130321     D*-------------------
007800130321     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
007900130321     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
008000130321
008100130318
008200130318     D pPathOrig       s            120A
008300130318     D pPathCli        s                   like(§AR8PPDFP)
008400130426     D pNomePDF        s             61A
008500130416     D pCheckOnly      s              1A
008600130318     D pFileName       s            256A
008700130318     D pEsito          s              1A
008800130318
008900130318     D dar8§pdf      e ds
009000130321     D wFileName       s            256A   inz(*blanks)
009100130318     D wPathName       s            256A   inz(*blanks)
009200130318     D wContaFile      s              1S 0 inz
009300130318     D wEsito          s              1A   inz
009400130321
009500130321     D wPreg           s               *
009600130321     D wNmatch         s             10U 0 INZ(2)
009700130321     D wPmatch         s               *
009800130321     D wPattern        s            256A
009900130321     D wStringa        s            256A
010000130318     D wDiraddr        s               *
010100130318     D wFiladdr        s               *
010200130318     D wRv             s             10I 0
010300130318
010400130318     D wDirent         ds                  based(wFiladdr)
010500130318     D   d_reserv1                   16A
010600130318     D   d_reserv2                   10U 0
010700130318     D   d_fileno                    10U 0
010800130318     D   d_reclen                    10U 0
010900130318     D   d_reserv3                   10I 0
011000130318     D   d_reserv4                    8A
011100130318     D   d_nlsinfo                   12A
011200130318     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
011300130318     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
011400130318     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
011500130318     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
011600130318     D   d_namelen                   10U 0
011700130318     D   d_name                     640A
011800120619
011900110617
012000110617
012100110627     C     *ENTRY        PLIST
012200130318     C                   PARM                    pPathOrig
012300130318     C                   PARM                    pPathCli
012400130318     C                   PARM                    pNomePDF
012500130416     C                   PARM                    pCheckOnly
012600130318     C                   PARM                    pFileName
012700130318     C                   PARM                    pEsito
012800130318     C*
012900130318     C* Inizializzo il flag di errore "locale" a OK
013000130318     C                   eval      wEsito = '0'
013100130318     C*
013200130318     C* Inizializzo i parametri di output
013300130318     C                   eval      pFileName = *blanks
013400130318     C                   eval      pEsito    = '2'
013500130318     C*
013600130318     C* Inizializzo il contatore di file che rispondono alla maschera ricevuta
013700130318     C                   eval      wContaFile = *zeros
013800130318     C*
013900130318     C* Apro la directory/maschera-file da ricercare
014000130321     C***                eval      wPathName =  %trim(pPathOrig) + '/' +
014100130321     C***                                       %trim(pPathCli)  + '/' +
014200130321     C***                                       %trim(pNomePDF)  + x'00'
014300130924     C                   if        pPathCli <> *blanks
014400130321     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
014500130321     C                                          %trim(pPathCli)  + x'00'
014600130924     C                   else
014700130924     C                   eval      wPathName =  %trim(pPathOrig) + x'00'
014800130924     C                   endif
014900130318     C                   eval      wDiraddr  = opendir(%addr(wPathName))
015000130318     C                   if        wDiraddr  = *NULL
015100130318     C*
015200130318     C* Se errore directory/maschera-file => flag errore "locale" a KO
015300130318     C                   eval      wEsito = '1'
015400130318     C                   else
015500130321     C*
015600130321     C* Effettuo le sostituzioni dei caratteri jolly
015700130321     C                   z-add     *zeros        wPos              3 0
015800130321     C                   eval      wPattern = %trim(pNomePDF)
015900130321     C                   eval      wPattern = %xlate(minu:maiu:wPattern)
016000130321     C                   eval      wPos = %scan('*':wPattern:1)
016100130321     C                   dow       wPos > *zero
016200130419     C                   eval      wPattern = %replace('.*':wPattern:wPos:1)
016300130419     C                   eval      wPos = %scan('*':wPattern:wPos+2)
016400130321     C                   enddo
016500130318     C*
016600130318     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
016700130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
016800130318     C                   eval      wFiladdr = readdir(wDiraddr)
016900130321     C                   dow       wFiladdr <> *NULL
017000130321     C*
017100130321     C* Se già conteggiato + di 1 file che corrisponde al nome => esco
017200130321     C                   if        wContaFile > 1
017300130321     C                   leave
017400130321     C                   endif
017500130416     C*
017600130416     C* Se richiesto solo controllo esistenza e 1 file già trivato => esco
017700130416     C                   if        pCheckOnly = 'Y' and wContaFile = 1
017800130416     C                   leave
017900130416     C                   endif
018000130318     C*
018100130318     C* Reperisco il nome completo del file corrente
018200130321     C                   eval      wFileName = %subst(d_name:1:d_namelen)
018300130321     C*
018400130321     C* Escludo le cartelle inferiore e superiore
018500130321     C                   if        %trim(wFileName) <> '.'  AND
018600130321     C                             %trim(wFileName) <> '..'
018700130321     C                   eval      wStringa = wFileName
018800130321     C                   eval      wStringa = %xlate(minu:maiu:wStringa)
018900130321     C*
019000130321     C* Compile init
019100130321     C                   clear                   regex_t
019200130321     C                   clear                   regmatch_t
019300130321     C     1             occur     regmatch_t
019400130321     C                   eval      wPmatch = %addr(regmatch_t)
019500130321     C                   eval      wPreg = %addr(regex_t)
019600130321     C                   eval      wPattern = %trim(wPattern) + x'00'
019700130321     C                   eval      wStringa = %trim(wStringa) + x'00'
019800130321     C*
019900130321     C* Compile RE
020000130321     C                   eval      wRv = regcomp(wPreg:%addr(wPattern):1)
020100130321     C                   if        wRv = *zeros
020200130321     C*
020300130321     C* Execute RE
020400130321     C                   eval      wRv = regexec(wPreg:%addr(wStringa):
020500130321     C                                           wNmatch:wPmatch: 0)
020600130321     C*
020700130321     C* Se Regular Expression corrente è soddisfatta ...
020800130321     C                   if        wRv = 0
020900130318     C*
021000130318     C* Conteggio oggetti rispondenti alla maschera-file ricevuta
021100130318     C                   eval      wContaFile = wContaFile + 1
021200130321     C*
021300130321     C* Salvo il nome completo
021400130321     C                   eval      pFileName = wFileName
021500130321     C*
021600130321     C                   endif
021700130321     C                   endif
021800130321     C*
021900130321     C* Scarico la Regular Expression corrente
022000130321     C                   callp     regfree(wPreg)
022100130321     C*
022200130321     C                   endif
022300130318     C*
022400130318     C* Procedo con la lettura di tutti i file che corrispondono alla
022500130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
022600130318     C                   eval      wFiladdr = readdir(wDiraddr)
022700130318     C                   enddo
022800130318     C*
022900130318     C* Chiudo la directory/maschera-file da ricercare
023000130318     C                   eval      wRv = closedir(wDiraddr)
023100130321     C                   endif
023200130318     C*
023300130318     C***************************
023400130318     C* Considerazioni finali
023500130318     C***************************
023600130318     C                   select
023700130318     C* PDF non trovato => ritento
023800130318     C                   when      wEsito = '1'
023900130318     C                   eval      pEsito = '2'
024000130405     C                   eval      pFileName = *blanks
024100130318     C* PDF non trovato => ritento
024200130318     C                   when      wContaFile = *zeros
024300130318     C                   eval      pEsito = '2'
024400130405     C                   eval      pFileName = *blanks
024500130318     C* PDF trovato singolo => tutto OK
024600130318     C                   when      wContaFile = 1
024700130318     C                   eval      pEsito = '0'
024800130318     C* PDF trovato non singolo => errore definitivo
024900130318     C                   when      wContaFile > 1
025000130318     C                   eval      pEsito = '3'
025100130405     C***                eval      pFileName = *blanks
025200130318     C                   endsl
025300130318     C*
025400130318     C                   seton                                        LR
025500130318     C
