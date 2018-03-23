000100060418      *PARMS CLOSQLCSR(*ENDMOD)
000200060418      *PARMS RDB(AS777)
000300060418      *PARMS USER(VAS777)
000400060418      *PARMS PASSWORD(wwwVAS)
000500050104      **********************************************************************************************
000600050104      *
000700050104      *  ATTENZIONE!!!
000800050104      *  X compilare questo programma occorre specificare i seguenti valori nei seguenti parametri:
000900050105      *  - compilare direttamente sul sistema AS/400 in cui dovrà girare
001000050105      *  - libreria destinazione oggetto compilato => GAITRAOBJ
001100060418      *  - RDB       => AS777
001200060418      *  - USER      => VAS777
001300060418      *  - PASSWORD  => wwwVAS
001400060418      *  - CLOSQLCSR => *ENDMOD
001500060418      *  Compilare con opzione 'PC'
001600050104      **********************************************************************************************
001700991021      /TITLE Reperimento user number di SGI.
001800991105     H dftactgrp(*yes)
001900991021
002000050104     D as777usr        s             10    INZ('SGISECOFR')
002100050104     D as777pwd        s             10    INZ('uffa')
002200100906     D prmusr          s             40
002300000717     D prmpwd          s             20
002400000719     D prmrag          s             20
002500000719     D prmrsp          s             30
002600000717     D prmsun          s              9
002700001116     D prmeml          s             64
002800050104     D prmesito        s              1
002900000717     D esito           s              1
003000001116     D operazione      s              1
003100000717     D api             s              1    inz('''')
003200001116     D cmd             s            500
003300060418     D cmdl            s             15  5 inz(%size(cmd))
003400060418
003500991021
003600060418     C*
003700060418     C* Per zelo disconnetto subito il sistema remoto sul quale andrò poi a connettermi
003800060418     C*
003900060418     C/EXEC SQL
004000060418     C+ DISCONNECT AS777
004100060418     C/END-EXEC
004200060418     C
004300060418     C*
004400060418     C* Connetto il sistema remoto
004500060418     C*
004600050104     C/EXEC SQL
004700050104     C+ CONNECT TO AS777 USER :as777usr USING :as777pwd
004800050104     C/END-EXEC
004900050104     C
005000060418     C*
005100060418     C* Eseguo call a *pgm remoto
005200050104     C
005300050104     C/EXEC SQL
005400050104     C+ CALL STRATEOBJ/TIS180R ('U', :prmsun, :prmusr, :prmesito)
005500050104     C/END-EXEC
005600050104     C*
005700050104     C* Testo eventuale errore nell'esecuzione remota
005800050104     C                   if        sqlcod < *zeros
005900050104     C                   movel     '1'           esito
006000050104     C                   endif
006100050104     C*
006200060418     C* Se tutto ok procedo
006300050104     C                   if        esito <> '1'
006400001116     C                   if        operazione = 'A'
006500001116     C                   if        prmsun <> *blanks
006600050104     C                   eval      cmd = 'STRATEGI/CHGSGIUSR ' +
006700001116     C                                               prmsun + '  ' +
006800001116     C                               'NAME(' + api + prmusr + api + ') ' +
006900001116     C                               'PASS(' + api + prmpwd + api + ') ' +
007000001116     C                              'FNAME(' + api + prmrag + api + ') ' +
007100001116     C                              'LNAME(' + api + prmrsp + api + ') ' +
007200001116     C                              'EMAIL(' + api + prmeml + api + ') '
007300060418     C*
007400060418     C* Eseguo comando remoto
007500060418     C*
007600060418     C/EXEC SQL
007700060418     C+ CALL  QSYS/QCMDEXC (:cmd, :cmdl)
007800060418     C/END-EXEC
007900060418     C*
008000060418     C* Testo eventuale errore nell'esecuzione remota
008100060418     C                   if        sqlcod < *zeros
008200060418     C                   movel     '1'           esito
008300060418     C                   endif
008400060418     C*
008500001116     C                   else
008600001116     C                   movel     '2'           esito
008700001116     C                   endif
008800001116     C                   endif
008900001116     C*
009000001116     C                   if        operazione = 'I'
009100000717     C                   if        prmsun = *blanks
009200050104     C                   eval      cmd = 'CALL STRATEOBJ/TIS180C PARM(' +
009300000717     C                                   api + prmusr + api + ' ' +
009400000717     C                                   api + prmpwd + api + ' ' +
009500000717     C                                   api + prmrag + api + ' ' +
009600000717     C                                   api + prmrsp + api + ' ' +
009700000717     C                                   api + prmsun + api + ' ' +
009800001116     C                                   api + 'U'    + api + ' ' +
009900001116     C                                   api + prmeml + api + ')'
010000060418     C*
010100060418     C* Eseguo comando remoto
010200060418     C*
010300060418     C/EXEC SQL
010400060418     C+ CALL  QSYS/QCMDEXC (:cmd, :cmdl)
010500060418     C/END-EXEC
010600060418     C*
010700060418     C* Testo eventuale errore nell'esecuzione remota
010800060418     C                   if        sqlcod < *zeros
010900060418     C                   movel     '1'           esito
011000060418     C                   endif
011100060418     C*
011200000717     C                   else
011300000717     C                   movel     '2'           esito
011400000717     C                   endif
011500050104     C
011600060418     C*
011700060418     C* Eseguo call a *pgm remoto
011800060418     C*
011900050104     C/EXEC SQL
012000050104     C+ CALL STRATEOBJ/TIS180R ('U', :prmsun, :prmusr, :prmesito)
012100050104     C/END-EXEC
012200050104     C*
012300050104     C* Testo eventuale errore nell'esecuzione remota
012400050104     C                   if        sqlcod < *zeros
012500050104     C                   movel     '1'           esito
012600050104     C                   endif
012700050104     C*
012800001116     C                   endif
012900050104     C                   endif
013000060418     C*
013100060418     C* Disconnetto il sistema remoto su cui mi sono già connesso
013200060418     C*
013300060418     C/EXEC SQL
013400060418     C+ DISCONNECT AS777
013500060418     C/END-EXEC
013600080125     C*
013700080125     C* Quindi riconnetto il DB sul sistema locale
013800080125     C*
013900080125     C/EXEC SQL
014000080125     C+ CONNECT RESET
014100080125     C/END-EXEC
014200080125     C*
014300080125     C/EXEC SQL
014400080125     C+ SET CONNECTION SETRAS
014500080125     C/END-EXEC
014600000717     C*
014700000717     C                   seton                                        lr
014800000717
014900060418
015000060418
015100060418     C     *pssr         BEGSR
015200060418     C*
015300060418     C* X qualunque errore nn monitorato esplicitamente x sicurezza imposto lo stato su errore
015400060418     C                   eval      esito = '1'
015500060418     C*
015600060418     C* E x sicurrezza disconnetto anche l'eventuale sistema remoto su cui mi sono già connesso
015700060418     C*
015800060418     C/EXEC SQL
015900060418     C+ DISCONNECT AS777
016000060418     C/END-EXEC
016100080125     C*
016200080125     C* Quindi riconnetto il DB sul sistema locale
016300080125     C*
016400080125     C/EXEC SQL
016500080125     C+ CONNECT RESET
016600080125     C/END-EXEC
016700080125     C*
016800080125     C/EXEC SQL
016900080125     C+ SET CONNECTION SETRAS
017000080125     C/END-EXEC
017100060418     C*
017200060418     C*
017300060418     C* Infine stronco il modulo corrente
017400060418     C                   ENDSR     '*CANCL'
017500060418     C***
017600060418
017700060418
017800000717
017900991021     C     *inzsr        begsr
018000000714     C*
018100000714     C     *entry        plist
018200000714     C                   parm                    prmusr
018300000717     C                   parm                    prmpwd
018400000717     C                   parm                    prmrag
018500000717     C                   parm                    prmrsp
018600000717     C                   parm                    prmsun
018700000717     C                   parm                    esito
018800001116     C                   parm                    operazione
018900001116     C                   parm                    prmeml
019000991021     C*
019100991021     C                   endsr
