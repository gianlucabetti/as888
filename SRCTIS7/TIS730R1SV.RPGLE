000100991021      /TITLE Reperimento user number di SGI.
000200991105     H dftactgrp(*yes)
000300991021
000400000717     Fsgius     if   e           k disk    usropn
000500991021
000600000714     D prmusr          s              7
000700000717     D prmpwd          s             20
000800000719     D prmrag          s             20
000900000719     D prmrsp          s             30
001000000717     D prmsun          s              9
001100001116     D prmeml          s             64
001200000717     D esito           s              1
001300001116     D operazione      s              1
001400000717     D api             s              1    inz('''')
001500001116     D cmd             s            500
001600991021
001700991021     C*
001800000717     C                   exsr      scanfile
001900001116     C*
002000001116     C                   if        operazione = 'A'
002100001116     C                   if        prmsun <> *blanks
002200001116     C                   eval      cmd = *blanks
002300001116     C                   eval      cmd = %trim(cmd) +
002400001116     C                                   'STRATEGI/CHGSGIUSR ' +
002500001116     C                                               prmsun + '  ' +
002600001116     C                               'NAME(' + api + prmusr + api + ') ' +
002700001116     C                               'PASS(' + api + prmpwd + api + ') ' +
002800001116     C                              'FNAME(' + api + prmrag + api + ') ' +
002900001116     C                              'LNAME(' + api + prmrsp + api + ') ' +
003000001116     C                              'EMAIL(' + api + prmeml + api + ') '
003100001116     C                   call(e)   'TIS730C'
003200001116     C                   parm                    cmd
003300001116     C                   if        %error
003400001116     C                   movel     '1'           esito
003500001116     C                   endif
003600001116     C                   else
003700001116     C                   movel     '2'           esito
003800001116     C                   endif
003900001116     C                   endif
004000001116     C*
004100001116     C                   if        operazione = 'I'
004200000717     C                   if        prmsun = *blanks
004300000717     C                   eval      cmd = *blanks
004400000717     C                   eval      cmd = %trim(cmd) +
004500000717     C                                   'CALL STRATEOBJ/TIS180C PARM(' +
004600000717     C                                   api + prmusr + api + ' ' +
004700000717     C                                   api + prmpwd + api + ' ' +
004800000717     C                                   api + prmrag + api + ' ' +
004900000717     C                                   api + prmrsp + api + ' ' +
005000000717     C                                   api + prmsun + api + ' ' +
005100001116     C                                   api + 'U'    + api + ' ' +
005200001116     C                                   api + prmeml + api + ')'
005300000717     C                   call(e)   'TIS730C'
005400000717     C                   parm                    cmd
005500000717     C                   if        %error
005600000717     C                   movel     '1'           esito
005700000717     C                   endif
005800000717     C                   else
005900000717     C                   movel     '2'           esito
006000000717     C                   endif
006100041229     C*                  if        esito = *blanks
006200041229     C                   exsr      scanfile
006300041229     C*                  if        prmsun <> *blanks
006400041229     C*                  eval      cmd = *blanks
006500041229     C*                  eval      cmd = %trim(cmd) +
006600041229     C*                                  'CALL STRATEOBJ/TIS180C PARM(' +
006700041229     C*                                  api + prmusr + api + ' ' +
006800041229     C*                                  api + prmpwd + api + ' ' +
006900041229     C*                                  api + prmrag + api + ' ' +
007000041229     C*                                  api + prmrsp + api + ' ' +
007100041229     C*                                  api + prmsun + api + ' ' +
007200041229     C*                                  api + 'Z'    + api + ' ' +
007300041229     C*                                  api + prmeml + api + ')'
007400041229     C*                  call(e)   'TIS730C'
007500041229     C*                  parm                    cmd
007600041229     C*                  if        %error
007700041229     C*                  movel     '1'           esito
007800041229     C*                  endif
007900041229     C*                  endif
008000041229     C*                  endif
008100001116     C                   endif
008200000717     C*
008300000717     C                   seton                                        lr
008400000717
008500000717
008600000717
008700000717     C*--------------------
008800000717     C* scanfile - Routine di scansione del file degli UTENTI di STRATEGI
008900000717     C*--------------------
009000000717     C     scanfile      begsr
009100000717     C*
009200000717     C                   open      sgius
009300000717     C*
009400000717     C                   read(e)   sgius
009500000714     C                   dow       not %eof
009600000714     C                   if        usanam = prmusr
009700000714     C                   eval      prmsun = ususer
009800000714     C                   leave
009900000714     C                   endif
010000000717     C                   read(e)   sgius
010100000714     C                   enddo
010200000717     C*
010300000717     C                   close     sgius
010400000714     C*
010500000717     C                   endsr
010600991021
010700000717
010800000717
010900000717
011000991021     C     *inzsr        begsr
011100000714     C*
011200000714     C     *entry        plist
011300000714     C                   parm                    prmusr
011400000717     C                   parm                    prmpwd
011500000717     C                   parm                    prmrag
011600000717     C                   parm                    prmrsp
011700000717     C                   parm                    prmsun
011800000717     C                   parm                    esito
011900001116     C                   parm                    operazione
012000001116     C                   parm                    prmeml
012100991021     C*
012200991021     C                   endsr
