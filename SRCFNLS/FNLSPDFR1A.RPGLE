000100110617      *---------------------------------------------------------------*
000200170125     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000300170125     H dftactgrp(*no) actgrp(*caller)
000400170125     H bnddir('UBBNDDIR':'QC2LE')
000500110617      *---------------------------------------------------------------*
000600130318
000700130318     D*--------------------------------------------------------------------
000800130318     D* Apre una directory
000900130318     D*
001000130318     D* opendir(string af a dirname) ==> return memory *address of the directory
001100130318     D*--------------------------------------------------------------------
001200130318     D opendir         PR              *   EXTPROC('opendir')
001300130318     D                                 *   VALUE
001400130318     D*--------------------------------------------------------------------
001500130318
001600130318     D*--------------------------------------------------------------------
001700130318     D* Chiude una directory
001800130318     D*
001900130318     D* closedir(string af a dirname) ==> return a value of operation's result
002000130318     D*--------------------------------------------------------------------
002100130318     D closedir        PR            10I 0 EXTPROC('closedir')
002200130318     D                                 *   VALUE
002300130318     D*--------------------------------------------------------------------
002400130318
002500130318     D*--------------------------------------------------------------------
002600130318     D* Legge una directory
002700130318     D*
002800130318     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
002900130318     D*--------------------------------------------------------------------
003000130318     D readdir         PR              *   EXTPROC('readdir')
003100130318     D                                 *   VALUE
003200130318     D*--------------------------------------------------------------------
003300170201
003400170201     D*--------------------------------------------------------------------
003500170201     D* Compone e Verifica Regular Expression
003600170201     D*
003700170201     D*--------------------------------------------------------------------
003800170201     D regcomp         PR            10I 0 EXTPROC('regcomp')
003900170201     D   preg                          *   VALUE
004000170201     D   pattern                       *   VALUE
004100170201     D   cflags                      10I 0 VALUE
004200170201     D*
004300170201     D regexec         PR            10I 0 EXTPROC('regexec')
004400170201     D   preg                          *   VALUE
004500170201     D   string                        *   VALUE
004600170201     D   nmatch                      10U 0 VALUE
004700170201     D   pmatch                        *   VALUE
004800170201     D   eflags                      10I 0 VALUE
004900170201     D*
005000170201     D regfree         PR                  EXTPROC('regfree')
005100170201     D   preg                          *   VALUE
005200170201     D*
005300170201     D regex_t         DS                  align
005400170201     D  re_nsub                      10I 0
005500170201     D  re_comp                        *
005600170201     D  re_cflags                    10I 0
005700170201     D  re_erroff                    10I 0
005800170201     D  re_len                       10I 0
005900170201     D  re_ucoll                     10I 0 DIM(2)
006000170201     D  re_lsub                        *   DIM(9)
006100170201     D  re_esub                        *   DIM(9)
006200170201     D  re_map                      256A
006300170201     D  re_shift                      5I 0
006400170201     D  re_dbcs                       5I 0
006500170201     D*
006600170201     D regmatch_t      DS                  occurs(2) align
006700170201     D  rm_so                        10I 0
006800170201     D  rm_ss                         5I 0
006900170201     D  rm_eo                        10I 0
007000170201     D  rm_es                         5I 0
007100170201     D*--------------------------------------------------------------------
007200130321
007300130321     D*-------------------
007400130321     D* COSTANTI
007500130321     D*-------------------
007600130321     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
007700130321     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
007800130321
007900130318
008000130318     D pPathOrig       s            120A
008100130318     D pPathCli        s                   like(§AR8PPDFP)
008200130426     D pNomePDF        s             61A
008300130416     D pCheckOnly      s              1A
008400130318     D pFileName       s            256A
008500130318     D pEsito          s              1A
008600130318
008700130318     D dar8§pdf      e ds
008800130321     D wFileName       s            256A   inz(*blanks)
008900130318     D wPathName       s            256A   inz(*blanks)
009000170125     D wFullFilNam     s            256A   inz(*blanks)
009100130318     D wContaFile      s              1S 0 inz
009200130318     D wEsito          s              1A   inz
009300130321
009400170201     D wPreg           s               *
009500170201     D wNmatch         s             10U 0 INZ(2)
009600170201     D wPmatch         s               *
009700170123     D wPattern        s            256A
009800170123     D wStringa        s            256A
009900130318     D wDiraddr        s               *
010000130318     D wFiladdr        s               *
010100130318     D wRv             s             10I 0
010200130318
010300130318     D wDirent         ds                  based(wFiladdr)
010400130318     D   d_reserv1                   16A
010500130318     D   d_reserv2                   10U 0
010600130318     D   d_fileno                    10U 0
010700130318     D   d_reclen                    10U 0
010800130318     D   d_reserv3                   10I 0
010900130318     D   d_reserv4                    8A
011000130318     D   d_nlsinfo                   12A
011100130318     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
011200130318     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
011300130318     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
011400130318     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
011500130318     D   d_namelen                   10U 0
011600130318     D   d_name                     640A
011700120619
011800170125     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
011900170125     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
012000170125
012100170201
012200170123
012300170123     C*
012400110627     C     *ENTRY        PLIST
012500130318     C                   PARM                    pPathOrig
012600130318     C                   PARM                    pPathCli
012700170125     C                   PARM                    pNomePDF
012800170125     C                   PARM                    pCheckOnly
012900130318     C                   PARM                    pFileName
013000130318     C                   PARM                    pEsito
013100130318     C*
013200130318     C* Inizializzo il flag di errore "locale" a OK
013300130318     C                   eval      wEsito = '0'
013400130318     C*
013500130318     C* Inizializzo i parametri di output
013600130318     C                   eval      pFileName = *blanks
013700130318     C                   eval      pEsito    = '2'
013800170125     C*
013900170125     C* Compongo gli elementi relativi a percorso e nome file
014000170125     C                   exsr      MkFullFileNam
014100170125     C*
014200170125     C* Verifico presenza wildcards nel nome file da verificare
014300170125     C                   if        %scan('*':pNomePDF) > *zeros
014400170125     C                   exsr      chkGeneric
014500170125     C                   else
014600170125     C                   exsr      chkSingle
014700170125     C                   endif
014800130318     C*
014900170125     C* Considerazioni finali
015000130318     C                   select
015100130318     C* PDF non trovato => ritento
015200130318     C                   when      wEsito = '1'
015300130318     C                   eval      pEsito = '2'
015400130405     C                   eval      pFileName = *blanks
015500130318     C* PDF non trovato => ritento
015600130318     C                   when      wContaFile = *zeros
015700130318     C                   eval      pEsito = '2'
015800130405     C                   eval      pFileName = *blanks
015900130318     C* PDF trovato singolo => tutto OK
016000130318     C                   when      wContaFile = 1
016100130318     C                   eval      pEsito = '0'
016200130318     C* PDF trovato non singolo => errore definitivo
016300130318     C                   when      wContaFile > 1
016400130318     C                   eval      pEsito = '3'
016500130405     C***                eval      pFileName = *blanks
016600130318     C                   endsl
016700130318     C*
016800130318     C                   seton                                        LR
016900130318     C
017000170125
017100170125
017200170125
017300170125     C     MkFullFileNam BEGSR
017400170125     C*
017500170125     C* Inizializzo il contatore di file che rispondono alla maschera ricevuta
017600170125     C                   eval      wContaFile = *zeros
017700170125     C*
017800170125     C* Compongo il percorso da aprire in IFS
017900170125     C                   if        pPathCli <> *blanks
018000170125     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
018100170125     C                                          %trim(pPathCli)  + x'00'
018200170125     C                   else
018300170125     C                   eval      wPathName =  %trim(pPathOrig) + x'00'
018400170125     C                   endif
018500170125     C*
018600170125     C* Compongo il percorso\nome file completo
018700170125     C                   eval      wFullFilNam = %trim(pPathOrig) + '/' +
018800170125     C                                           %trim(pPathCli)  + '/' +
018900170125     C                                           %trim(pNomePDF)
019000170125     C*
019100170125     C* Normalizzo percorso rispetto agli slah presenti
019200170125     C                   eval      wFullFilNam =
019300170125     C                             %scanrpl('//':'/':wFullFilNam)
019400170125     C*
019500170125     C                   ENDSR
019600170125
019700170125
019800170125
019900170125        BEGSR chkSingle;
020000170125
020100170125          // Verifico la dimensione ovvero la presenza del file in IFS
020200170125          if UBIFSUTIR_GetFileSizeByte(wFullFilNam
020300170125                                   :oIFSUTIsizeByte) < 0;
020400170125             wEsito = '1';
020500170125          else;
020600170125             if oIFSUTIsizeByte > *zeros;
020700170125                wContaFile = 1;
020800170201                pFileName = %trim(pNomePDF);
020900170125             endif;
021000170125          endif;
021100170125
021200170125        ENDSR;
021300170125
021400170125
021500170125
021600170125     C     chkGeneric    BEGSR
021700170125     C*
021800170125     C* Apro la directory/maschera-file da ricercare
021900170125     C                   eval      wDiraddr  = opendir(%addr(wPathName))
022000170125     C                   if        wDiraddr  = *NULL
022100170125     C*
022200170125     C* Se errore directory/maschera-file => flag errore "locale" a KO
022300170125     C                   eval      wEsito = '1'
022400170125     C                   else
022500170125     C*
022600170125     C* Effettuo le sostituzioni dei caratteri jolly
022700170125     C                   z-add     *zeros        wPos              3 0
022800170125     C                   eval      wPattern = %trim(pNomePDF)
022900170125     C                   eval      wPattern = %xlate(minu:maiu:wPattern)
023000170125     C                   eval      wPos = %scan('*':wPattern:1)
023100170125     C                   dow       wPos > *zero
023200170125     C                   eval      wPattern = %replace('.*':wPattern:wPos:1)
023300170125     C                   eval      wPos = %scan('*':wPattern:wPos+2)
023400170125     C                   enddo
023500170125     C*
023600170125     C* Imposto il pattern aggiungendo gli "indicatori" di inizio e fine riga
023700170125     C                   eval      wPattern = '^' + %trim(wPattern) + '$'
023800170201     C                   eval      wPattern = %trim(wPattern) + x'00'
023900170125     C*
024000170125     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
024100170125     C* maschera file ricevuta in input - nella cartella ricevuta in input
024200170125     C                   eval      wFiladdr = readdir(wDiraddr)
024300170125     C                   dow       wFiladdr <> *NULL
024400170125     C*
024500170125     C* Se già conteggiato + di 1 file che corrisponde al nome => esco
024600170125     C                   if        wContaFile > 1
024700170125     C                   leave
024800170125     C                   endif
024900170125     C*
025000170125     C* Se richiesto solo controllo esistenza e 1 file già trovato => esco
025100170125     C                   if        pCheckOnly = 'Y' and wContaFile = 1
025200170125     C                   leave
025300170125     C                   endif
025400170125     C*
025500170125     C* Reperisco il nome completo del file corrente
025600170125     C                   eval      wFileName = %subst(d_name:1:d_namelen)
025700170125     C*
025800170125     C* Escludo le cartelle inferiore e superiore
025900170125     C                   if        %trim(wFileName) <> '.'  AND
026000170125     C                             %trim(wFileName) <> '..'
026100170125     C                   eval      wStringa = wFileName
026200170125     C                   eval      wStringa = %xlate(minu:maiu:wStringa)
026300170201     C                   eval      wStringa = %trim(wStringa) + x'00'
026400170201     C*
026500170201     C* Compile init
026600170201     C                   clear                   regex_t
026700170201     C                   clear                   regmatch_t
026800170201     C     1             occur     regmatch_t
026900170201     C                   eval      wPmatch = %addr(regmatch_t)
027000170201     C                   eval      wPreg = %addr(regex_t)
027100170201     C*
027200170201     C* Compile RE
027300170201     C                   eval      wRv = regcomp(wPreg:%addr(wPattern):1)
027400170201     C                   if        wRv = *zeros
027500170201     C*
027600170201     C* Execute RE
027700170201     C                   eval      wRv = regexec(wPreg:%addr(wStringa):
027800170201     C                                           wNmatch:wPmatch: 0)
027900170201     C*
028000170201     C* Se Regular Expression corrente è soddisfatta ...
028100170201     C                   if        wRv = 0
028200170201     C*
028300170201     C* Conteggio oggetti rispondenti alla maschera-file ricevuta
028400170201     C                   eval      wContaFile = wContaFile + 1
028500170201     C*
028600170201     C* Salvo il nome completo
028700170201     C                   eval      pFileName = wFileName
028800170201     C*
028900170201     C                   endif
029000170201     C                   endif
029100170201     C*
029200170201     C* Scarico la Regular Expression corrente
029300170201     C                   callp     regfree(wPreg)
029400170201     C*
029500170201     C                   endif
029600170125     C*
029700170125     C* Procedo con la lettura di tutti i file che corrispondono alla
029800170125     C* maschera file ricevuta in input - nella cartella ricevuta in input
029900170125     C                   eval      wFiladdr = readdir(wDiraddr)
030000170125     C                   enddo
030100170125     C*
030200170125     C* Chiudo la directory/maschera-file da ricercare
030300170125     C                   eval      wRv = closedir(wDiraddr)
030400170125     C                   endif
030500170125     C*
030600170125     C                   ENDSR
