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
003600130321     D*-------------------
003700130321     D* COSTANTI
003800130321     D*-------------------
003900130321     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004000130321     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004100130321
004200130318
004300130318     D pPathOrig       s            120A
004400130318     D pPathCli        s                   like(§AR8PPDFP)
004500130426     D pNomePDF        s             61A
004600130416     D pCheckOnly      s              1A
004700130318     D pFileName       s            256A
004800130318     D pEsito          s              1A
004900130318
005000130318     D dar8§pdf      e ds
005100130321     D wFileName       s            256A   inz(*blanks)
005200130318     D wPathName       s            256A   inz(*blanks)
005300130318     D wContaFile      s              1S 0 inz
005400130318     D wEsito          s              1A   inz
005500170123     D wSQL            s           2048    inz  varying
005600170123     D wApice          c                   const('''')
005700170123     D wTrovato        s              1A   inz
005800130321
005900170123     D wPattern        s            256A
006000170123     D wStringa        s            256A
006100130318     D wDiraddr        s               *
006200130318     D wFiladdr        s               *
006300130318     D wRv             s             10I 0
006400130318
006500130318     D wDirent         ds                  based(wFiladdr)
006600130318     D   d_reserv1                   16A
006700130318     D   d_reserv2                   10U 0
006800130318     D   d_fileno                    10U 0
006900130318     D   d_reclen                    10U 0
007000130318     D   d_reserv3                   10I 0
007100130318     D   d_reserv4                    8A
007200130318     D   d_nlsinfo                   12A
007300130318     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
007400130318     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
007500130318     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
007600130318     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
007700130318     D   d_namelen                   10U 0
007800130318     D   d_name                     640A
007900120619
008000110617
008100170123
008200170123     C*
008300170123     C* Definisco le opzioni con cui verranno utilizzate le istruzioni SQL
008400170123     C
008500170123     C/EXEC SQL
008600170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008700170123     C/END-EXEC
008800170123     C*
008900170123     C*
009000110627     C     *ENTRY        PLIST
009100130318     C                   PARM                    pPathOrig
009200130318     C                   PARM                    pPathCli
009300130318     C                   PARM                    pNomePDF
009400130416     C                   PARM                    pCheckOnly
009500130318     C                   PARM                    pFileName
009600130318     C                   PARM                    pEsito
009700130318     C*
009800130318     C* Inizializzo il flag di errore "locale" a OK
009900130318     C                   eval      wEsito = '0'
010000130318     C*
010100130318     C* Inizializzo i parametri di output
010200130318     C                   eval      pFileName = *blanks
010300130318     C                   eval      pEsito    = '2'
010400130318     C*
010500130318     C* Inizializzo il contatore di file che rispondono alla maschera ricevuta
010600130318     C                   eval      wContaFile = *zeros
010700130318     C*
010800130318     C* Apro la directory/maschera-file da ricercare
010900130924     C                   if        pPathCli <> *blanks
011000130321     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
011100130321     C                                          %trim(pPathCli)  + x'00'
011200130924     C                   else
011300130924     C                   eval      wPathName =  %trim(pPathOrig) + x'00'
011400130924     C                   endif
011500130318     C                   eval      wDiraddr  = opendir(%addr(wPathName))
011600130318     C                   if        wDiraddr  = *NULL
011700130318     C*
011800130318     C* Se errore directory/maschera-file => flag errore "locale" a KO
011900130318     C                   eval      wEsito = '1'
012000130318     C                   else
012100130321     C*
012200130321     C* Effettuo le sostituzioni dei caratteri jolly
012300130321     C                   z-add     *zeros        wPos              3 0
012400130321     C                   eval      wPattern = %trim(pNomePDF)
012500130321     C                   eval      wPattern = %xlate(minu:maiu:wPattern)
012600130321     C                   eval      wPos = %scan('*':wPattern:1)
012700130321     C                   dow       wPos > *zero
012800130419     C                   eval      wPattern = %replace('.*':wPattern:wPos:1)
012900130419     C                   eval      wPos = %scan('*':wPattern:wPos+2)
013000130321     C                   enddo
013100170123     C*
013200170123     C* Imposto il pattern aggiungendo gli "indicatori" di inizio e fine riga
013300170123     C                   eval      wPattern = '^' + %trim(wPattern) + '$'
013400130318     C*
013500130318     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
013600130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
013700130318     C                   eval      wFiladdr = readdir(wDiraddr)
013800130321     C                   dow       wFiladdr <> *NULL
013900130321     C*
014000130321     C* Se già conteggiato + di 1 file che corrisponde al nome => esco
014100130321     C                   if        wContaFile > 1
014200130321     C                   leave
014300130321     C                   endif
014400130416     C*
014500130416     C* Se richiesto solo controllo esistenza e 1 file già trivato => esco
014600130416     C                   if        pCheckOnly = 'Y' and wContaFile = 1
014700130416     C                   leave
014800130416     C                   endif
014900130318     C*
015000130318     C* Reperisco il nome completo del file corrente
015100130321     C                   eval      wFileName = %subst(d_name:1:d_namelen)
015200130321     C*
015300130321     C* Escludo le cartelle inferiore e superiore
015400130321     C                   if        %trim(wFileName) <> '.'  AND
015500130321     C                             %trim(wFileName) <> '..'
015600130321     C                   eval      wStringa = wFileName
015700130321     C                   eval      wStringa = %xlate(minu:maiu:wStringa)
015800170123     C*
015900170123     C* Applica la regular expression definita nel pattern alla stringa corrente
016000170123     C                   eval      wTrovato = *zero
016100170123
016200170123       // Preparo l'istruzione SQL che declina la Regular Expression
016300170123       wSQL = 'SELECT ' + wApice + '1' + wApice +
016400170123              ' from SYSIBM.SYSDUMMY1 WHERE REGEXP_LIKE(' +
016500170123              wApice + %trim(wStringa) + wApice + ', ' +
016600170123              wApice + %trim(wPattern) + wApice + ') ';
016700170123
016800170123       // Dichiarazione cursore
016900170123       exec sql   prepare S0   from :wSQL;
017000170123       exec sql   declare C0   cursor   for S0;
017100170123
017200170123       // Apertura del cursore
017300170123       exec sql   open C0;
017400170123
017500170123       // Lettura cursore
017600170123       exec sql   fetch next   from C0   into :wTrovato;
017700170123
017800130321     C*
017900130321     C* Se Regular Expression corrente è soddisfatta ...
018000170123     C                   if        wTrovato = *on
018100130318     C*
018200130318     C* Conteggio oggetti rispondenti alla maschera-file ricevuta
018300130318     C                   eval      wContaFile = wContaFile + 1
018400130321     C*
018500130321     C* Salvo il nome completo
018600130321     C                   eval      pFileName = wFileName
018700130321     C*
018800130321     C                   endif
018900130321     C*
019000130321     C                   endif
019100130318     C*
019200130318     C* Procedo con la lettura di tutti i file che corrispondono alla
019300130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
019400130318     C                   eval      wFiladdr = readdir(wDiraddr)
019500130318     C                   enddo
019600130318     C*
019700130318     C* Chiudo la directory/maschera-file da ricercare
019800130318     C                   eval      wRv = closedir(wDiraddr)
019900130321     C                   endif
020000130318     C*
020100130318     C***************************
020200130318     C* Considerazioni finali
020300130318     C***************************
020400130318     C                   select
020500130318     C* PDF non trovato => ritento
020600130318     C                   when      wEsito = '1'
020700130318     C                   eval      pEsito = '2'
020800130405     C                   eval      pFileName = *blanks
020900130318     C* PDF non trovato => ritento
021000130318     C                   when      wContaFile = *zeros
021100130318     C                   eval      pEsito = '2'
021200130405     C                   eval      pFileName = *blanks
021300130318     C* PDF trovato singolo => tutto OK
021400130318     C                   when      wContaFile = 1
021500130318     C                   eval      pEsito = '0'
021600130318     C* PDF trovato non singolo => errore definitivo
021700130318     C                   when      wContaFile > 1
021800130318     C                   eval      pEsito = '3'
021900130405     C***                eval      pFileName = *blanks
022000130318     C                   endsl
022100130318     C*
022200130318     C                   seton                                        LR
022300130318     C
