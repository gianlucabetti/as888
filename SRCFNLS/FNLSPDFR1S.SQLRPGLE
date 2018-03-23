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
003300130321
003400130321     D*-------------------
003500130321     D* COSTANTI
003600130321     D*-------------------
003700130321     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
003800130321     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
003900130321
004000130318
004100130318     D pPathOrig       s            120A
004200130318     D pPathCli        s                   like(§AR8PPDFP)
004300130426     D pNomePDF        s             61A
004400130416     D pCheckOnly      s              1A
004500130318     D pFileName       s            256A
004600130318     D pEsito          s              1A
004700130318
004800130318     D dar8§pdf      e ds
004900130321     D wFileName       s            256A   inz(*blanks)
005000130318     D wPathName       s            256A   inz(*blanks)
005100170125     D wFullFilNam     s            256A   inz(*blanks)
005200130318     D wContaFile      s              1S 0 inz
005300130318     D wEsito          s              1A   inz
005400170123     D wSQL            s           2048    inz  varying
005500170123     D wApice          c                   const('''')
005600170123     D wTrovato        s              1A   inz
005700130321
005800170123     D wPattern        s            256A
005900170123     D wStringa        s            256A
006000130318     D wDiraddr        s               *
006100130318     D wFiladdr        s               *
006200130318     D wRv             s             10I 0
006300130318
006400130318     D wDirent         ds                  based(wFiladdr)
006500130318     D   d_reserv1                   16A
006600130318     D   d_reserv2                   10U 0
006700130318     D   d_fileno                    10U 0
006800130318     D   d_reclen                    10U 0
006900130318     D   d_reserv3                   10I 0
007000130318     D   d_reserv4                    8A
007100130318     D   d_nlsinfo                   12A
007200130318     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
007300130318     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
007400130318     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
007500130318     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
007600130318     D   d_namelen                   10U 0
007700130318     D   d_name                     640A
007800120619
007900170125     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
008000170125     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
008100170125
008200110617
008300170123
008400170123     C*
008500170123     C* Definisco le opzioni con cui verranno utilizzate le istruzioni SQL
008600170123     C
008700170123     C/EXEC SQL
008800170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008900170123     C/END-EXEC
009000170123     C*
009100170123     C*
009200110627     C     *ENTRY        PLIST
009300130318     C                   PARM                    pPathOrig
009400130318     C                   PARM                    pPathCli
009500170125     C                   PARM                    pNomePDF
009600170125     C                   PARM                    pCheckOnly
009700130318     C                   PARM                    pFileName
009800130318     C                   PARM                    pEsito
009900130318     C*
010000130318     C* Inizializzo il flag di errore "locale" a OK
010100130318     C                   eval      wEsito = '0'
010200130318     C*
010300130318     C* Inizializzo i parametri di output
010400130318     C                   eval      pFileName = *blanks
010500130318     C                   eval      pEsito    = '2'
010600170125     C*
010700170125     C* Compongo gli elementi relativi a percorso e nome file
010800170125     C                   exsr      MkFullFileNam
010900170125     C*
011000170125     C* Verifico presenza wildcards nel nome file da verificare
011100170125     C                   if        %scan('*':pNomePDF) > *zeros
011200170125     C                   exsr      chkGeneric
011300170125     C                   else
011400170125     C                   exsr      chkSingle
011500170125     C                   endif
011600130318     C*
011700170125     C* Considerazioni finali
011800130318     C                   select
011900130318     C* PDF non trovato => ritento
012000130318     C                   when      wEsito = '1'
012100130318     C                   eval      pEsito = '2'
012200130405     C                   eval      pFileName = *blanks
012300130318     C* PDF non trovato => ritento
012400130318     C                   when      wContaFile = *zeros
012500130318     C                   eval      pEsito = '2'
012600130405     C                   eval      pFileName = *blanks
012700130318     C* PDF trovato singolo => tutto OK
012800130318     C                   when      wContaFile = 1
012900130318     C                   eval      pEsito = '0'
013000130318     C* PDF trovato non singolo => errore definitivo
013100130318     C                   when      wContaFile > 1
013200130318     C                   eval      pEsito = '3'
013300130405     C***                eval      pFileName = *blanks
013400130318     C                   endsl
013500130318     C*
013600130318     C                   seton                                        LR
013700130318     C
013800170125
013900170125
014000170125
014100170125     C     MkFullFileNam BEGSR
014200170125     C*
014300170125     C* Inizializzo il contatore di file che rispondono alla maschera ricevuta
014400170125     C                   eval      wContaFile = *zeros
014500170125     C*
014600170125     C* Compongo il percorso da aprire in IFS
014700170125     C                   if        pPathCli <> *blanks
014800170125     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
014900170125     C                                          %trim(pPathCli)  + x'00'
015000170125     C                   else
015100170125     C                   eval      wPathName =  %trim(pPathOrig) + x'00'
015200170125     C                   endif
015300170125     C*
015400170125     C* Compongo il percorso\nome file completo
015500170125     C                   eval      wFullFilNam = %trim(pPathOrig) + '/' +
015600170125     C                                           %trim(pPathCli)  + '/' +
015700170125     C                                           %trim(pNomePDF)
015800170125     C*
015900170125     C* Normalizzo percorso rispetto agli slah presenti
016000170125     C                   eval      wFullFilNam =
016100170125     C                             %scanrpl('//':'/':wFullFilNam)
016200170125     C*
016300170125     C                   ENDSR
016400170125
016500170125
016600170125
016700170125        BEGSR chkSingle;
016800170125
016900170125          // Verifico la dimensione ovvero la presenza del file in IFS
017000170125          if UBIFSUTIR_GetFileSizeByte(wFullFilNam
017100170125                                   :oIFSUTIsizeByte) < 0;
017200170125             wEsito = '1';
017300170125          else;
017400170125             if oIFSUTIsizeByte > *zeros;
017500170125                wContaFile = 1;
017600170125             endif;
017700170125          endif;
017800170125
017900170125        ENDSR;
018000170125
018100170125
018200170125
018300170125     C     chkGeneric    BEGSR
018400170125     C*
018500170125     C* Apro la directory/maschera-file da ricercare
018600170125     C                   eval      wDiraddr  = opendir(%addr(wPathName))
018700170125     C                   if        wDiraddr  = *NULL
018800170125     C*
018900170125     C* Se errore directory/maschera-file => flag errore "locale" a KO
019000170125     C                   eval      wEsito = '1'
019100170125     C                   else
019200170125     C*
019300170125     C* Effettuo le sostituzioni dei caratteri jolly
019400170125     C                   z-add     *zeros        wPos              3 0
019500170125     C                   eval      wPattern = %trim(pNomePDF)
019600170125     C                   eval      wPattern = %xlate(minu:maiu:wPattern)
019700170125     C                   eval      wPos = %scan('*':wPattern:1)
019800170125     C                   dow       wPos > *zero
019900170125     C                   eval      wPattern = %replace('.*':wPattern:wPos:1)
020000170125     C                   eval      wPos = %scan('*':wPattern:wPos+2)
020100170125     C                   enddo
020200170125     C*
020300170125     C* Imposto il pattern aggiungendo gli "indicatori" di inizio e fine riga
020400170125     C                   eval      wPattern = '^' + %trim(wPattern) + '$'
020500170125     C*
020600170125     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
020700170125     C* maschera file ricevuta in input - nella cartella ricevuta in input
020800170125     C                   eval      wFiladdr = readdir(wDiraddr)
020900170125     C                   dow       wFiladdr <> *NULL
021000170125     C*
021100170125     C* Se già conteggiato + di 1 file che corrisponde al nome => esco
021200170125     C                   if        wContaFile > 1
021300170125     C                   leave
021400170125     C                   endif
021500170125     C*
021600170125     C* Se richiesto solo controllo esistenza e 1 file già trovato => esco
021700170125     C                   if        pCheckOnly = 'Y' and wContaFile = 1
021800170125     C                   leave
021900170125     C                   endif
022000170125     C*
022100170125     C* Reperisco il nome completo del file corrente
022200170125     C                   eval      wFileName = %subst(d_name:1:d_namelen)
022300170125     C*
022400170125     C* Escludo le cartelle inferiore e superiore
022500170125     C                   if        %trim(wFileName) <> '.'  AND
022600170125     C                             %trim(wFileName) <> '..'
022700170125     C                   eval      wStringa = wFileName
022800170125     C                   eval      wStringa = %xlate(minu:maiu:wStringa)
022900170125     C*
023000170125     C* Applica la regular expression definita nel pattern alla stringa corrente
023100170125     C                   eval      wTrovato = *zero
023200170125
023300170125       // Preparo l'istruzione SQL che declina la Regular Expression
023400170125       wSQL = 'SELECT ' + wApice + '1' + wApice +
023500170125              ' from SYSIBM.SYSDUMMY1 WHERE REGEXP_LIKE(' +
023600170125              wApice + %trim(wStringa) + wApice + ', ' +
023700170125              wApice + %trim(wPattern) + wApice + ') ';
023800170125
023900170125       // Dichiarazione cursore
024000170125       exec sql   prepare S0   from :wSQL;
024100170125       exec sql   declare C0   cursor   for S0;
024200170125
024300170125       // Apertura del cursore
024400170125       exec sql   open C0;
024500170125
024600170125       // Lettura cursore
024700170125       exec sql   fetch next   from C0   into :wTrovato;
024800170125
024900170125       // Chiusura cursore
025000170125       exec sql   close C0;
025100170125
025200170125     C*
025300170125     C* Se Regular Expression corrente è soddisfatta ...
025400170125     C                   if        wTrovato = *on
025500170125     C*
025600170125     C* Conteggio oggetti rispondenti alla maschera-file ricevuta
025700170125     C                   eval      wContaFile = wContaFile + 1
025800170125     C*
025900170125     C* Salvo il nome completo
026000170125     C                   eval      pFileName = wFileName
026100170125     C*
026200170125     C                   endif
026300170125     C*
026400170125     C                   endif
026500170125     C*
026600170125     C* Procedo con la lettura di tutti i file che corrispondono alla
026700170125     C* maschera file ricevuta in input - nella cartella ricevuta in input
026800170125     C                   eval      wFiladdr = readdir(wDiraddr)
026900170125     C                   enddo
027000170125     C*
027100170125     C* Chiudo la directory/maschera-file da ricercare
027200170125     C                   eval      wRv = closedir(wDiraddr)
027300170125     C                   endif
027400170125     C*
027500170125     C                   ENDSR
