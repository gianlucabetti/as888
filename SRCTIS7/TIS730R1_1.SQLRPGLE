000100050104      **********************************************************************************************
000200050104      *
000300050104      *  ATTENZIONE!!!
000400050104      *  X compilare questo programma occorre specificare i seguenti valori nei seguenti parametri:
000500050105      *  - compilare direttamente sul sistema AS/400 in cui dovrà girare
000600050105      *  - libreria destinazione oggetto compilato => GAITRAOBJ
000700050104      *  - RDB => AS777
000800050104      *  - USER => VAS777
000900050104      *  - PASSWORD => wwwVAS
001000050104      *
001100050104      **********************************************************************************************
001200991021      /TITLE Reperimento user number di SGI.
001300991105     H dftactgrp(*yes)
001400991021
001500050104     D as777usr        s             10    INZ('SGISECOFR')
001600050104     D as777pwd        s             10    INZ('uffa')
001700000714     D prmusr          s              7
001800000717     D prmpwd          s             20
001900000719     D prmrag          s             20
002000000719     D prmrsp          s             30
002100000717     D prmsun          s              9
002200001116     D prmeml          s             64
002300050104     D prmesito        s              1
002400000717     D esito           s              1
002500001116     D operazione      s              1
002600000717     D api             s              1    inz('''')
002700001116     D cmd             s            500
002800991021
002900050104     C/EXEC SQL
003000050104     C+ CONNECT TO AS777 USER :as777usr USING :as777pwd
003100050104     C/END-EXEC
003200050104     C
003300050104     C
003400050104     C/EXEC SQL
003500050104     C+ CALL STRATEOBJ/TIS180R ('U', :prmsun, :prmusr, :prmesito)
003600050104     C/END-EXEC
003700050104     C*
003800050104     C* Testo eventuale errore nell'esecuzione remota
003900050104     C                   if        sqlcod < *zeros
004000050104     C                   movel     '1'           esito
004100050104     C                   endif
004200050104     C
004300050104     C/EXEC SQL
004400050104     C+ DISCONNECT AS777
004500050104     C/END-EXEC
004600050104     C
004700050104     C*
004800050104     C                   if        esito <> '1'
004900001116     C                   if        operazione = 'A'
005000001116     C                   if        prmsun <> *blanks
005100050104     C                   eval      cmd = 'STRATEGI/CHGSGIUSR ' +
005200001116     C                                               prmsun + '  ' +
005300001116     C                               'NAME(' + api + prmusr + api + ') ' +
005400001116     C                               'PASS(' + api + prmpwd + api + ') ' +
005500001116     C                              'FNAME(' + api + prmrag + api + ') ' +
005600001116     C                              'LNAME(' + api + prmrsp + api + ') ' +
005700001116     C                              'EMAIL(' + api + prmeml + api + ') '
005800001116     C                   call(e)   'TIS730C'
005900001116     C                   parm                    cmd
006000001116     C                   if        %error
006100001116     C                   movel     '1'           esito
006200001116     C                   endif
006300001116     C                   else
006400001116     C                   movel     '2'           esito
006500001116     C                   endif
006600001116     C                   endif
006700001116     C*
006800001116     C                   if        operazione = 'I'
006900000717     C                   if        prmsun = *blanks
007000050104     C                   eval      cmd = 'CALL STRATEOBJ/TIS180C PARM(' +
007100000717     C                                   api + prmusr + api + ' ' +
007200000717     C                                   api + prmpwd + api + ' ' +
007300000717     C                                   api + prmrag + api + ' ' +
007400000717     C                                   api + prmrsp + api + ' ' +
007500000717     C                                   api + prmsun + api + ' ' +
007600001116     C                                   api + 'U'    + api + ' ' +
007700001116     C                                   api + prmeml + api + ')'
007800000717     C                   call(e)   'TIS730C'
007900000717     C                   parm                    cmd
008000000717     C                   if        %error
008100000717     C                   movel     '1'           esito
008200000717     C                   endif
008300000717     C                   else
008400000717     C                   movel     '2'           esito
008500000717     C                   endif
008600050104     C
008700050104     C/EXEC SQL
008800050104     C+ CONNECT TO AS777 USER :as777usr USING :as777pwd
008900050104     C/END-EXEC
009000050104     C
009100050104     C
009200050104     C/EXEC SQL
009300050104     C+ CALL STRATEOBJ/TIS180R ('U', :prmsun, :prmusr, :prmesito)
009400050104     C/END-EXEC
009500050104     C*
009600050104     C* Testo eventuale errore nell'esecuzione remota
009700050104     C                   if        sqlcod < *zeros
009800050104     C                   movel     '1'           esito
009900050104     C                   endif
010000050104     C
010100050104     C/EXEC SQL
010200050104     C+ DISCONNECT AS777
010300050104     C/END-EXEC
010400050104     C
010500050104     C*
010600001116     C                   endif
010700050104     C                   endif
010800000717     C*
010900000717     C                   seton                                        lr
011000000717
011100000717
011200000717
011300991021     C     *inzsr        begsr
011400000714     C*
011500000714     C     *entry        plist
011600000714     C                   parm                    prmusr
011700000717     C                   parm                    prmpwd
011800000717     C                   parm                    prmrag
011900000717     C                   parm                    prmrsp
012000000717     C                   parm                    prmsun
012100000717     C                   parm                    esito
012200001116     C                   parm                    operazione
012300001116     C                   parm                    prmeml
012400991021     C*
012500991021     C                   endsr
