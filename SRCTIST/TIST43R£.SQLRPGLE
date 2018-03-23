000100000000     H*------------------------------------------------------------------------*
000200071228     H* CREAZIONE FILE DATI CLIENTI/FORNITORI PER POSTEL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600000925     FAZORG01L  IF   E           K DISK
000700000928     FYBACL01L  IF   E           K DISK
000800000922     FWFPTL01L  UF A E           K DISK    USROPN
000900000922     FWFPTL02L  UF A E           K DISK    USROPN
001000000922     F                                     RENAME(WFPTL000:WFPTL002)
001100071231     F                                     INFDS(WFPTLINF)
001200011016     FWFPTL03L  UF   E           K DISK    USROPN
001300011016     F                                     RENAME(WFPTL000:WFPTL003)
001400011127     FWFPTL04L  UF   E           K DISK    USROPN
001500011127     F                                     RENAME(WFPTL000:WFPTL004)
001600030306     FWFPTL05L  UF   E           K DISK    USROPN
001700030306     F                                     RENAME(WFPTL000:WFPTL005)
001800060222     FWFPTL07L  UF   E           K DISK    USROPN
001900060222     F                                     RENAME(WFPTL000:WFPTL007) PREFIX(K_)
002000071231     FWFPTL00F  UF   E             DISK    USROPN
002100011127     F                                     RENAME(WFPTL000:WFPTLFFF)
002200071231     F                                     PREFIX(F_)
002300130829     fAZNTC01L  if   e           k disk
002400020925     FTFNTC01L  IF   E           K DISK
002500020311     FANRCO98J  IF   E           K DISK
002600050406     FANRCO01L  IF   E           K DISK    rename(ANRCO000:ANRCO001) prefix(F_)
002700020311     FANCLN01L  IF   E           K DISK
002800000000     D*------------------------------------------------------------------------*
002900000000     D* SCHIERE
003000000000     D*------------------------------------------------------------------------*
003100000925     D*------------
003200000925     D* FILIALI "VALIDE"
003300000925     D*------------
003400000925     D FILAGE          s              3  0 DIM(300)                             * FILIALI/AGENZIE
003500000920     D*------------
003600000920     D* COMANDI
003700000920     D*------------
003800040902     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*-------------------
004300000000     D* ARCHITETTURA
004400000000     D*-------------------
004500000000     D KPJBA         E DS
004600050404     D*-------------------
004700050404     D* DS X REPERIMENTO DATI INTRADAMENTO TRAMITE TISI95R
004800050404     D*-------------------
004900050404     D TISI95DS      E DS
005000000000     D*-------------------
005100000922     D* DS SCOMPOSIZIONE DATE INIZ/FIN
005200000922     D*-------------------
005300000922     D                 DS                  INZ
005400000928     D  DIANN                  1      4
005500000928     D  DIMES                  5      6
005600000928     D  DIMGS                  5      8
005700060717     D  DIDAT6                 1      6
005800000928     D  DIDAT                  1      8
005900000922     D                 DS                  INZ
006000000928     D  DFANN                  1      4
006100000928     D  DFMES                  5      6
006200000928     D  DFMGS                  5      8
006300060717     D  DFDAT6                 1      6
006400000928     D  DFDAT                  1      8
006500000000     D*-------------------
006600000000     D* DS PARAMETRI DI LANCIO PGM
006700000000     D*-------------------
006800060714     D PARAM           DS
006900060714     D  PARDAI                        8  0
007000060714     D  PARDAF                        8  0
007100060714     D  PARFAT                       17  0
007200060714     D  PARKSC                        7
007300060714     D  PARFIL                        3  0
007400060714     D  PARNAZ                        3
007500060714     D  PARCAP                        9
007600060714     D  PARPRV                        2
007700060714     D  PARBLK                        1
007800060714     D  PARCIC                        1
007900060714     D  PARBAR                        1
008000060714     D  PARSDI                        1
008100060714     D  PARPIC                        1
008200060714     D  PARFOA                        1
008300060714     D  PARNTN1                       2
008400060714     D  PARNTE1                       2
008500060714     D  PARSOC                        3
008600060714     D  PARCOF                        1
008700060714     D  PARCPI                        1
008800060714     D  PARIND                        1
008900060714     D  PARNTN2                       2
009000060714     D  PARNTN3                       2
009100060714     D  PARNTN4                       2
009200060714     D  PARNTE2                       2
009300060714     D  PARNTE3                       2
009400060714     D  PARNTE4                       2
009500060714     D  PARNTE5                       2
009600060714     D  PARNTE6                       2
009700060714     D  PARNTE7                       2
009800060714     D  PARNTE8                       2
009900060714     D  PARTBC                        1
010000060714     D  PARRGR                        1
010100060714     D  PAROVR                        1
010200060714     D  PARCHK                        1
010300060714     D  PARSOGN                       1
010400000914     D*-------------------
010500000922     D* DS DI LETTURA RECORDSET DEI SALDI VIA SQL
010600000914     D*-------------------
010700000922     D DSSALDI         DS
010800000922     D  §SDOKSC                1      7  0
010900000922     D  §SDOFAT                8     22  3
011000050404     D*-------------------
011100050404     D* DS DI LETTURA RECORDSET DEL FATTURATO FORNITORI VIA SQL
011200050404     D*-------------------
011300050404     D DSSALDIFORN     DS
011400050404     D  §IVASOCIETA                   3
011500050404     D  §IVAKCCCP                     6
011600050404     D  §IVAKSCCP                     8
011700050404     D  §IVAFATTURATO                11  2
011800050404     D  §IVADATAULTFT                10D
011900050404     D  §IVANOTAACCR                 11  2
012000050404     D  §IVADATAULTNA                10D
012100000922     D*-------------------
012200050405     D* DS DI LETTURA RECORDSET DELLE ANAGRAFICHE SOGGETTI (PRO-J) VIA SQL
012300000922     D*-------------------
012400000922     D DSPJANA         DS
012500000922     D  §PJSOC                 1      3
012600000922     D  §PJSOG                 4     11
012700000922     D  §PJKSC                12     19
012800000922     D  §PJDES                20     63
012900000922     D  §PJIVA                64     83
013000000922     D  §PJCDF                84    103
013100000922     D  §PJIND               104    147
013200000922     D  §PJLOC               148    181
013300000922     D  §PJCAP               182    190
013400000922     D  §PJPRV               191    192
013500000922     D  §PJNAZ               193    196
013600000925     D  §PJAVA               197    199
013700000925     D  §PJCLF               200    207
013800001110     D  §PJABC               208    208
013900001114     D  §PJTEL               209    228
014000020311     D  §PJKCC               229    234
014100060615     D  §PJDTINVL            235    244
014200010914     D*-------------------
014300010914     D* DS DI RIDEFINIZIONE KSC DI PRO-J
014400010914     D*-------------------
014500010914     D DSPJKSC         DS
014600010914     D  WPJFL1                        1
014700010914     D  WPJFIL                        3  0
014800010914     D  WPJFL2                        4
014900071228     D*------------------
015000071228     D* DS REPERIMENTO CLIENTI UNIFICANTI/STAND-ALONE
015100071228     D*-------------------
015200071228     D TIBS10DS      E DS
015300071231     D*------------------
015400071231     D* DS INFO FILE WFPTL02L
015500071231     D*-------------------
015600071231     D wfptlinf        ds
015700071231     D  wfptlnrri            397    400i 0
015800071231     D wfptlnrrp       s             10s 0
015900000914     D*------------------------------------------------------------------------*
016000000918     D* VARIABILI DI WRK
016100000914     D*------------------------------------------------------------------------*
016200000922     D  SQLline        s           5000    varying
016300001211     D  SQLline2       s           5000    varying
016400000922     D  SQLcond        s           5000    varying
016500001211     D  SQLcond2       s           5000    varying
016600000922     D  SQLord         s           5000    varying
016700000926     D  SQLvar         s           5000    varying
016800060529     D  depemlr        s                   like(wfpeml) inz(*loval)
016900000922     D  depioc         s                   like(wfpioc) inz(*loval)
017000011127     D  depksc         s                   like(wfpksc) inz(*loval)
017100050404     D  depsoc         s                   like(wfpsoc) inz(*loval)
017200030306     D  depeml         s                   like(wfpeml) inz(*blanks)
017300030306     D  deprsp         s                   like(wfprsp) inz(*blanks)
017400050406     D  depfat         s                   like(wfpfat) inz(*zeros)
017500010131     D  idx            s              5i 0 inz(*zeros)
017600010131     D  Trovato        s              1
017700011016     D  countInd       s             10  0 inz(*zeros)
017800011016     D  wrkrag         s                   like(wfprag)
017900011016     D  wrkind         s                   like(wfpind)
018000011016     D  wrkloc         s                   like(wfploc)
018100011016     D  wrkpro         s                   like(wfppro)
018200011016     D  wrkcap         s                   like(wfpcap)
018300011016     D  wrknaz         s                   like(wfpnaz)
018400050404     D  wrknatura      s                   like(rcosnatura)
018500050404     D  didatforn      s             10D
018600050404     D  dfdatforn      s             10D
018700060222     D  w§PJCLF        s                   like(§PJCLF) inz(*zeros)
018800060616     D  findata        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
018900000922     D*------------------
019000000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
019100000921     D*------------------
019200000921     D WLBDA8          DS                  INZ
019300000921     D  G08DAT                 1      8  0
019400000921     D  G08INV                 9     16  0
019500000921     D  G08ERR                17     17
019600000921     D  G08TGI                18     22  0
019700020923     D*-------------------
019800020923     D* DS REPERIMENTO DATI UTENTE
019900020923     D*-------------------
020000020923     D TIBS34DS      E DS                                                       *Profili utente
020100020923     D DDATIUTE      E DS                                                       *Dati utente
020200020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
020300000000     C*------------------------------------------------------------------------*
020400000000     C* MAIN LINE
020500000000     C*------------------------------------------------------------------------*
020600071212     C*
020700071212     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
020800071212     C
020900071212     C/EXEC SQL
021000071212     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
021100071212     C/END-EXEC
021200000000     C*
021300000920     C                   exsr      procedi
021400050404     C*
021500050404     C* Effettuo lancio TISI95 solo x chiusura
021600050404     C                   clear                   TISI95DS
021700050404     C                   eval      I95TLA = 'C'
021800050404     C                   call      'TISI95R'
021900050404     C                   parm                    TISI95DS
022000000914     C*
022100000914     C                   SETON                                        LR
022200000922     C*------------------------------------------------------------------------*
022300000922     C* PROCEDI - Routine principale
022400000922     C*------------------------------------------------------------------------*
022500000922     C     PROCEDI       BEGSR
022600000922     C*
022700030307     C* CLRPFM del file (WFPTL00F)
022800030306     C                   Z-ADD     80            LENGH            15 5
022900071231     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023000030307     C                   PARM                    CMD(1)
023100030307     C                   PARM                    LENGH
023200071231     C*
023300071231     C* Apro il file (WFPTL00F)
023400071231     C                   open      wfptl00f
023500000922     C*
023600000922     C* Se riuscito comando ==> procedo
023700000922     C                   if        not %error
023800001211     C                   if        PARCOF = 'C'
023900030307     C                   exsr      exesald
024000001211     C                   endif
024100050404     C                   if        PARCOF = 'F'
024200050404     C                   exsr      exesaldforn
024300050404     C                   endif
024400001211     C                   eval      SQLvar = 'clnksc<>clnclifatt and '       +
024500000926     C                             'clnclifatt <> ''                '' and '
024600030307     C                   exsr      exeana
024700001211     C                   eval      SQLvar = '(clnksc=clnclifatt or '        +
024800001211     C                             'clnclifatt=''                '') and '
024900030307     C                   exsr      exeana
025000011016     C                   exsr      exechk
025100030307     C                   exsr      exeanaorg
025200030307     C                   exsr      exeinduniq
025300050406     C                   if        PARCOF = 'C'
025400050405     C                   exsr      exedel888999
025500050405     C                   endif
025600000922     C                   endif
025700071231     C*
025800071231     C* Chiudo il file (WFPTL00F)
025900071231     C                   close     wfptl00f
026000000922     C*
026100000922     C                   ENDSR
026200000914     C*------------------------------------------------------------------------*
026300000922     C* EXESALD - Routine di elaborazione saldi
026400000914     C*------------------------------------------------------------------------*
026500000922     C     EXESALD       BEGSR
026600000914     C*
026700000922     C* Apro il file (WFPTL01L)
026800000922     C                   open      wfptl01l
026900000914     C*
027000000914     C* Scompongo i parametri ricevuti in input
027100060717     C                   movel     PARDAI        didat6
027200060717     C                   movel     PARDAF        dfdat6
027300000914     C*
027400000914     C/EXEC SQL
027500000928     C+ declare C1 cursor for
027600000928     C+ select sdoksc, sum(sdoirb) from sisdo00f where
027700060717     C+ concat(digits(sdoann),digits(sdomes)) >= :didat6 and
027800060717     C+ concat(digits(sdoann),digits(sdomes)) <= :dfdat6
027900000928     C+ group by sdoksc
028000000928     C+ for read only
028100000914     C/END-EXEC
028200000914     C
028300000914     C/EXEC SQL
028400000914     C+ open C1
028500000914     C/END-EXEC
028600000914     C
028700000914     C/EXEC SQL
028800000922     C+ Fetch C1 into :dssaldi
028900000914     C/END-EXEC
029000000914     C*
029100000914     C                   dow       sqlcod = *zeros
029200000922     C                   clear                   wfptl000
029300050405     C                   movel     PARCOF        wfptip
029400050405     C                   move(p)   '201'         wfpsoc
029500000922     C                   z-add     §sdoksc       wfpksc
029600000922     C                   z-add     §sdofat       wfpfat
029700000922     C                   write     wfptl000
029800000914     C*
029900000914     C/EXEC SQL
030000000922     C+ Fetch C1 into :dssaldi
030100000914     C/END-EXEC
030200000914     C*
030300000914     C                   enddo
030400000914     C*
030500000914     C/EXEC SQL
030600000914     C+ close C1
030700000914     C/END-EXEC
030800000914     C
030900000914     C*
031000000922     C* Chiudo il file (WFPTL01L)
031100000922     C                   close     wfptl01l
031200000920     C*
031300000914     C                   ENDSR
031400050404     C*------------------------------------------------------------------------*
031500050404     C* EXESALDFORN - Routine di elaborazione saldi fornitori (da file IVA contabilità)
031600050404     C*------------------------------------------------------------------------*
031700050404     C     EXESALDFORN   BEGSR
031800050404     C*
031900050404     C* Apro il file (WFPTL01L)
032000050404     C                   open      wfptl01l
032100050404     C*
032200050404     C* Scompongo i parametri ricevuti in input
032300050404     C     *ISO          move      PARDAI        didatforn
032400050404     C     *ISO          move      PARDAF        dfdatforn
032500050404     C*
032600050404     C/EXEC SQL
032700050404     C+ declare C3 cursor for
032800050404     C+ SELECT F.IVASOCIETA, F.IVAKCCCP, F.IVAKSCCP,
032900050404     C+ DEC(SUM(F.IVAIMPONIB*(IVADARE-IVAAVERE)), 11, 2) FATTURATO,
033000050404     C+ MAX(F.IVADTDOC) ULTIMA_FT ,
033100050404     C+ MAX(IFNULL(N.NOTE_ACCREDITO, 0)) NOTE_ACCREDITO,
033200050404     C+ MAX(IFNULL(N.ULTIMA, '0001-01-01')) ULTIMA_NA
033300050404     C+ FROM NDIVA00F F LEFT JOIN (SELECT
033400050404     C+ IVASOCIETA, IVAKCCCP, IVAKSCCP,
033500050404     C+ DEC(SUM(IVAIMPONIB*(IVADARE-IVAAVERE)), 11, 2) NOTE_ACCREDITO,
033600050404     C+ MAX(IVADTDOC) ULTIMA FROM NDIVA00F JOIN ANOPE00F ON IVASOCIETA =
033700050404     C+ OPESOCIETA AND IVACAUSTES = OPECAUSALE WHERE IVATPREG = '1' AND
033800050404     C+ IVADTDOC >= :didatforn AND IVADTDOC <= :dfdatforn
033900050404     C+ AND IVADARE = 0 AND OPETPDOCI IN
034000050404     C+ ('1','4','7') GROUP BY IVASOCIETA, IVAKCCCP, IVAKSCCP)
034100050404     C+ N ON F.IVASOCIETA = N.IVASOCIETA
034200050404     C+ AND F.IVAKCCCP = N.IVAKCCCP AND F.IVAKSCCP = N.IVAKSCCP JOIN
034300050404     C+ ANOPE00F ON F.IVASOCIETA=OPESOCIETA AND F.IVACAUSTES=OPECAUSALE
034400050404     C+ WHERE F.IVATPREG = '1' AND
034500050404     C+ F.IVADTDOC >= :didatforn AND F.IVADTDOC <= :dfdatforn AND
034600050404     C+ F.IVADARE = 1 AND OPETPDOCI IN ('0','3','5')
034700050404     C+ GROUP BY F.IVASOCIETA,
034800050404     C+ F.IVAKCCCP, F.IVAKSCCP
034900050404     C+ ORDER BY
035000050404     C+ F.IVASOCIETA, F.IVAKCCCP, F.IVAKSCCP
035100050404     C+ for read only
035200050404     C/END-EXEC
035300050404     C
035400050404     C/EXEC SQL
035500050404     C+ open C3
035600050404     C/END-EXEC
035700050404     C
035800050404     C/EXEC SQL
035900050404     C+ Fetch C3 into :dssaldiforn
036000050404     C/END-EXEC
036100050404     C*
036200050404     C                   dow       sqlcod = *zeros
036300050404     C                   clear                   wfptl000
036400050405     C                   movel     PARCOF        wfptip
036500050405     C                   move(p)   §ivasocieta   wfpsoc
036600050404     C                   move(p)   §ivaksccp     wfpksc
036700050404     C                   eval      wfpfat = §ivafatturato +
036800050404     C                                      §ivanotaaccr
036900050404     C                   write     wfptl000
037000050404     C*
037100050404     C/EXEC SQL
037200050404     C+ Fetch C3 into :dssaldiforn
037300050404     C/END-EXEC
037400050404     C*
037500050404     C                   enddo
037600050404     C*
037700050404     C/EXEC SQL
037800050404     C+ close C3
037900050404     C/END-EXEC
038000050404     C
038100050404     C*
038200050404     C* Chiudo il file (WFPTL01L)
038300050404     C                   close     wfptl01l
038400050404     C*
038500050404     C                   ENDSR
038600000922     C*------------------------------------------------------------------------*
038700000922     C* EXEANA  - Routine di elaborazione anagrafico
038800000922     C*------------------------------------------------------------------------*
038900000922     C     EXEANA        BEGSR
039000000922     C*
039100060222     C* Apro i file (WFPTL01L e WFPTL07L)
039200000922     C                   open      wfptl01l
039300060222     C                   open      wfptl07l
039400000922     C*
039500001211     C* Inizializzo le variabili stringhe per la composizione dell'SQL
039600001211     C                   clear                   SQLline
039700001211     C                   eval      %len(SQLline) = 0
039800001211     C                   clear                   SQLline2
039900001211     C                   eval      %len(SQLline2)= 0
040000001211     C                   clear                   SQLcond
040100001211     C                   eval      %len(SQLcond) = 0
040200001211     C                   clear                   SQLcond2
040300001211     C                   eval      %len(SQLcond2)= 0
040400001211     C                   clear                   SQLord
040500001211     C                   eval      %len(SQLord)  = 0
040600001211     C*
040700001211     C                   eval      SQLline = 'select clnsocieta, clnsogg, '    +
040800000922     C                             'clnksc, sogdes, sogpartiva, sogcdfisc, '   +
040900000922     C                             'indindriz, indlocalit, indcap, indprov, '  +
041000001110     C                             'indstato, rcosocaval, clnclifatt, '        +
041100060615     C                             'clnclasabc, indtelefon, clnkcc, '          +
041200060615     C                             'sogdtinvl '                                +
041300000925     C                             'from (((ancln00f join ansog00f on '        +
041400001211     C                             'clnsogg=sogsogg) join anind00f on clnsogg' +
041500000925     C                             '=indsogg) join anrco00f on clnsocieta='    +
041600000925     C                             'rcosocieta and clnkcc=rcokcc and '         +
041700000926     C                             'clnksc=rcoksc) where '                     +
041800001211     C                             SQLvar                                      +
041900001211     C                             'rcosocaval=''   '' and indtpind=''  '' '   +
042000001211     C                             'and indcdind=''   '' '
042100000922     C*
042200001207     C                   if        PARCOF <> *blanks
042300001207     C                   eval      SQLcond = %trim(SQLcond)+
042400001207     C                                       ' and rcosnatura='+''''+PARCOF+''''
042500001207     C                   endif
042600001207     C                   if        PARKSC = *blanks
042700000927     C                   if        PARCAP<>*blanks
042800000927     C                   eval      SQLcond = %trim(SQLcond)+
042900001207     C                                       ' and indcap='+''''+PARCAP+''''
043000000927     C                   endif
043100000927     C                   if        PARPRV<>*blanks
043200000927     C                   eval      SQLcond = %trim(SQLcond)+
043300001207     C                                       ' and indprov='+''''+PARPRV+''''
043400000927     C                   endif
043500000927     C                   if        PARNAZ<>'999'
043600000927     C                   if        PARNAZ='   '
043700000927     C                   eval      SQLcond = %trim(SQLcond)+
043800001211     C                             ' and indstato IN (''IT  '', ''    '')'
043900000927     C                   else
044000000927     C                   eval      SQLcond = %trim(SQLcond)+
044100001207     C                                       ' and indstato='+''''+PARNAZ+''''
044200000927     C                   endif
044300000927     C                   endif
044400001207     C                   if        PARSOC <> *blanks
044500001207     C                   eval      SQLcond = %trim(SQLcond)+
044600001211     C                                       ' and rcosocieta='+''''+PARSOC+''''
044700001207     C                   else
044800000922     C                   if        PARBAR='S' and PARSDI='N' and PARPIC='N'
044900000927     C                   eval      SQLcond = %trim(SQLcond)+
045000001211     C                                       ' and (rcosocieta = ''201'')'
045100000922     C                   endif
045200000922     C                   if        PARBAR='S' and PARSDI='S' and PARPIC='N'
045300000927     C                   eval      SQLcond = %trim(SQLcond)+
045400001211     C                                       ' and (rcosocieta = ''201'' or ' +
045500001211     C                                       'rcosocieta = ''101'')'
045600000922     C                   endif
045700000922     C                   if        PARBAR='N' and PARSDI='S' and PARPIC='S'
045800000927     C                   eval      SQLcond = %trim(SQLcond)+
045900001211     C                                       ' and (rcosocieta <> ''201'')'
046000000922     C                   endif
046100000922     C                   if        PARBAR='N' and PARSDI='S' and PARPIC='N'
046200000927     C                   eval      SQLcond = %trim(SQLcond)+
046300001211     C                                       ' and (rcosocieta = ''101'')'
046400000922     C                   endif
046500000925     C                   if        PARBAR='N' and PARSDI='N' and PARPIC='S'
046600000927     C                   eval      SQLcond = %trim(SQLcond)+
046700001211     C                                       ' and (rcosocieta <> ''201'' '+
046800001211     C                                       'and  rcosocieta <> ''101'')'
046900000925     C                   endif
047000001207     C                   endif
047100001211     C                   eval      SQLcond2= %trim(SQLcond)
047200000927     C                   else
047300001211     C                   eval      SQLcond2= %trim(SQLcond)
047400000927     C                   eval      SQLcond = %trim(SQLcond)+
047500001207     C                                     ' and clnksc='+''''+'0'+PARKSC+''''
047600000927     C                   endif
047700000922     C*
047800001211     C                   eval      SQLline2 = 'union select frnsocieta, '      +
047900001211     C                             'frnsogg, frnksc, '                         +
048000001211     C                             'sogdes, sogpartiva, sogcdfisc, '           +
048100001211     C                             'indindriz, indlocalit, indcap, indprov, '  +
048200001211     C                             'indstato, rcosocaval, frnforfatt, '        +
048300060615     C                             ''' '', indtelefon, frnkcc, '               +
048400060615     C                             'sogdtinvl '                                +
048500001211     C                             'from (((anfrn00f join ansog00f on '        +
048600001211     C                             'frnsogg=sogsogg) join anind00f on frnsogg' +
048700001211     C                             '=indsogg) join anrco00f on frnsocieta='    +
048800001211     C                             'rcosocieta and frnkcc=rcokcc and '         +
048900001211     C                             'frnksc=rcoksc) where '                     +
049000001211     C                             'rcosocaval=''   '' and indtpind=''  '' '   +
049100001211     C                             'and indcdind=''   '' '
049200001211     C*
049300001211     C                   eval      SQLord = ' order by 5, 6'
049400000922     C*
049500001211     C                   eval      SQLline= %trim(SQLline) +' '+%trim(SQLcond) +
049600001211     C                                      ' '                                +
049700001211     C                                      %trim(SQLline2)+' '+%trim(SQLcond2)+
049800001211     C                                      ' '+%trim(SQLord)
049900000922     C*
050000000922     C/EXEC SQL
050100000922     C+ prepare S2 from :SQLline
050200000922     C/END-EXEC
050300000922     C*
050400000922     C*
050500000922     C/EXEC SQL
050600000922     C+ declare C2 cursor for S2
050700000922     C/END-EXEC
050800000922     C*
050900000922     C*
051000000922     C/EXEC SQL
051100000922     C+ open C2
051200000922     C/END-EXEC
051300000922     C*
051400000922     C*
051500000922     C/EXEC SQL
051600000922     C+ fetch C2 into :dspjana
051700000922     C/END-EXEC
051800000922     C*
051900000922     C                   dow       sqlcod = *zeros
052000000928     C                   exsr      chkrec
052100000928     C                   if        errl00 = '0'
052200000922     C                   exsr      valana
052300000928     C                   endif
052400000922     C*
052500000922     C/EXEC SQL
052600000922     C+ Fetch C2 into :dspjana
052700000922     C/END-EXEC
052800000922     C*
052900000922     C                   enddo
053000000922     C*
053100000922     C/EXEC SQL
053200000922     C+ close C2
053300000922     C/END-EXEC
053400000922     C
053500000922     C*
053600060222     C* Chiudo i file (WFPTL01L e WFPTL07L)
053700000922     C                   close     wfptl01l
053800060222     C                   close     wfptl07l
053900000922     C*
054000000922     C                   ENDSR
054100071228     C*------------------------------------------------------------------------*
054200071228     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
054300071228     C*------------------------------------------------------------------------*
054400071228     C     RTVPARENT     BEGSR
054500071228     C*
054600071228     C* Reperisco il padre del codice cliente corrente
054700071228     C                   CLEAR                   TIBS10DS
054800071228     C                   EVAL      D10TLE = 'ST'
054900071228     C                   EVAL      D10PAF = 'P'
055000071228     C                   MOVE(P)   §PJKSC        D10COD
055100071228     C                   CALL      'TIBS10R'
055200071228     C                   PARM                    TIBS10DS
055300071228     C                   PARM      'E'           TIPEXE            1
055400071228     C                   IF        D10ERR = '1'
055500071228     C                   Z-ADD     D10COD        §D10COP          11 0
055600071228     C                   ELSE
055700071228     C                   Z-ADD     D10COP        §D10COP
055800071228     C                   ENDIF
055900071228     C*
056000071228     C                   ENDSR
056100000922     C*------------------------------------------------------------------------*
056200000922     C* VALANA - Routine di considerazoini e valorizzazione campi anagrafici
056300000922     C*------------------------------------------------------------------------*
056400000922     C     VALANA        BEGSR
056500000922     C*
056600000926     C                   if        §PJAVA = *blanks
056700000926     C*
056800000925     C                   if        §PJCLF = *blanks
056900000925     C                   eval      §PJCLF = §PJKSC
057000000925     C                   endif
057100000925     C*
057200000926     C                   z-add     *zeros        contarec          1 0
057300000926     C*
057400050404     C                   move      §PJSOC        KWPSOC
057500000926     C                   move      §PJKSC        KWPKSC
057600050404     C     KEYPTL01      setll     wfptl01l
057700050404     C     KEYPTL01      reade     wfptl01l
057800000926     C                   dow       not %eof(wfptl01l)
057900000926     C                   z-add     1             contarec
058000060222     C                   if        wfpchk <> 'X'
058100071228     C                   eval      w§PJCLF = §PJKSC
058200071228     C                   if        (PAROVR = 'F' AND §PJKSC <> §PJCLF) OR
058300071228     C                             PAROVR = 'U'
058400050404     C                   exsr      ovrvalrec
058500011127     C                   endif
058600060222     C                   movel     PARCOF        wfptip
058700060222     C*
058800071228     C* Alla fine imposto il codice cliente da considerare (sia esso KSC/FAT/UNI)
058900071228     C                   move (p)  w§PJCLF       wfpksc
059000060222     C*
059100050405     C* Se lancio x clienti reperisco i dati del commerciale relativo
059200050405     C                   if        PARCOF = 'C'
059300011127     C                   exsr      exeage
059400081205     C                   exsr      exetbc
059500050405     C                   endif
059600050405     C*
059700011127     C                   exsr      valrec
059800000926     C                   update    wfptl000
059900060222     C                   endif
060000050404     C     KEYPTL01      reade     wfptl01l
060100000926     C                   enddo
060200000926     C*
060300000926     C                   if        contarec = *zeros
060400000926     C                   clear                   wfptl000
060500071228     C                   eval      w§PJCLF = §PJKSC
060600071228     C                   if        (PAROVR = 'F' AND §PJKSC <> §PJCLF) OR
060700071228     C                             PAROVR = 'U'
060800050404     C                   exsr      ovrvalrec
060900011127     C                   endif
061000050405     C                   movel     PARCOF        wfptip
061100071228     C*
061200071228     C* Alla fine imposto il codice cliente da considerare (sia esso KSC/FAT/UNI)
061300071228     C                   move (p)  w§PJCLF       wfpksc
061400060222     C*
061500050405     C* Se lancio x clienti reperisco i dati del commerciale relativo
061600050405     C                   if        PARCOF = 'C'
061700011127     C                   exsr      exeage
061800081205     C                   exsr      exetbc
061900050405     C                   endif
062000060222     C*
062100060222     C* Salvo sul record il tipo blocco cliente
062200071228     C                   eval      wfptbc = flgblk
062300060222     C*
062400060222     C* Eseguo aggiornamento x indicare il codice blocco su tutti i record aventi medesimo
062500071228     C* codice cliente (ogni codice cliente deve mantenere cmq il suo codice blocco,
062600071228     C* successivamente se tutti i codici clienti della stessa PIVA sono bloccati verranno
062700071231     C* tutti eliminati - vedi vista logica WFPTL02L).
062800071228     C*    wfpksc        setll     wfptl07l
062900071228     C*    wfpksc        reade     wfptl07l
063000071228     C*                  dow       not %eof(wfptl07l)
063100071228     C*                  eval      K_wfptbc = flgblk
063200071228     C*                  update    wfptl007
063300071228     C*    wfpksc        reade     wfptl07l
063400071228     C*                  enddo
063500050405     C*
063600000926     C                   exsr      valrec
063700000926     C                   write     wfptl000
063800000926     C                   endif
063900000926     C                   endif
064000000922     C*
064100000922     C                   ENDSR
064200000922     C*------------------------------------------------------------------------*
064300000922     C* VALREC - Routine di valorizzazione buffer del record
064400000922     C*------------------------------------------------------------------------*
064500000922     C     VALREC        BEGSR
064600050406     C*
064700050406     C* Solo se lancio x fornitori reperisco la classificazione attività
064800050406     C                   if        PARCOF = 'F'
064900050406     C     KEYRCO01L     chain     ANRCO01L
065000050406     C                   if        %found(ANRCO01L)
065100050406     C                   eval      §PJABC = %trim(F_RCOCTGAN02)
065200050406     C                   endif
065300050406     C                   endif
065400000922     C*
065500000922     C* Valorizzo solo i campi del buffer del record che mi interessano
065600000922     C                   movel(p)  §PJDES        wfprag
065700000922     C                   movel(p)  §PJIND        wfpind
065800000922     C                   movel(p)  §PJLOC        wfploc
065900000922     C                   movel(p)  §PJCAP        wfpcap
066000000922     C                   movel(p)  §PJPRV        wfppro
066100001110     C                   movel(p)  §PJABC        wfpabc
066200001114     C                   movel(p)  §PJTEL        wfptel
066300050404     C                   movel(p)  §PJSOC        wfpsoc
066400000922     C                   if        §PJNAZ='IT '
066500000922     C                   movel(p)  *blanks       wfpnaz
066600000922     C                   else
066700000922     C                   movel(p)  §PJNAZ        wfpnaz
066800000922     C                   endif
066900061114     C*
067000061114     C* X le successive considerazioni battezzo o la P.IVA o il COD.FISC.
067100071231     C                   if        §PJIVA <> *blanks AND
067200071231     C                             %trim(§PJIVA) <> '$$'
067300000922     C                   movel(p)  §PJIVA        wfpioc
067400000922     C                   else
067500000922     C                   movel(p)  §PJCDF        wfpioc
067600000922     C                   endif
067700061114     C*
067800061114     C* Ma memorizzo cmq entrambi i dati
067900061114     C                   movel(p)  §PJIVA        wfppiva
068000061114     C                   movel(p)  §PJCDF        wfpcodf
068100060615     C*
068200060616     C* Se richiesto nei parametri d lancio....
068300060616     C* ...determino se il soggetto è "nuovo" (ovvero se la data attivazione in anagrafica è +
068400060616     C* recente rispetto alla data "A" del periodo fatturazione indicato nei parametri d lancio,
068500060616     C* ri-ovvero dell'ultimo periodo saldi cliente)
068600060616     C                   if        PARSOGN = 'S'
068700060615     C                   movel     *blanks       wDataCliAttAlf    8
068800060615     C                   z-add     *zeros        wDataCliAttNum    8 0
068900060615     C                   eval      wDataCliAttAlf = %subst(§PJDTINVL: 1: 4) +
069000060615     C                                              %subst(§PJDTINVL: 6: 2) +
069100060615     C                                              %subst(§PJDTINVL: 9: 2)
069200060615     C                   move(p)   wDataCliAttAlfwDataCliAttNum
069300060616     C*
069400060616     C* Sottraggo 1 ulteriore mese dalla data "A"
069500060616     C     *ISO          MOVE      PARDAF        findata
069600060619     C     findata       SUBDUR    2:*M          findata
069700060616     C     *ISO          MOVE      findata       wDataNewSogg      8 0
069800060616     C*
069900060616     C* Verifico quindi se + recente
070000060616     C                   if        wDataCliAttNum >= wDataNewSogg
070100061114     C                   eval      wfpson = 'S'
070200060615     C                   endif
070300060616     C                   endif
070400010914     C*
070500050711     C* Solo se lancio x clienti effettuo le chain
070600050405     C                   if        PARCOF = 'C'
070700060619     C                   movel     *blanks       wSavTNT           2
070800060619     C                   movel     *blanks       wSavTNT_01        2
070900060619     C                   movel     *blanks       wSavTNT_02        2
071000060619     C                   movel     *blanks       wSavTNT_03        2
071100060619     C                   movel     *blanks       wSavTNT_04        2
071200060619     C                   movel     *blanks       wSavTNT_05        2
071300060619     C                   movel     *blanks       wSavTNT_06        2
071400060619     C                   movel     *blanks       wSavTNT_07        2
071500060619     C                   movel     *blanks       wSavTNT_08        2
071600060619     C                   movel     *blanks       wSavRNT_01       60
071700060619     C                   movel     *blanks       wSavRNT_02       60
071800060619     C                   movel     *blanks       wSavRNT_03       60
071900060619     C                   movel     *blanks       wSavRNT_04       60
072000060619     C                   movel     *blanks       wSavRNT_05       60
072100060619     C                   movel     *blanks       wSavRNT_06       60
072200060619     C                   movel     *blanks       wSavRNT_07       60
072300060619     C                   movel     *blanks       wSavRNT_08       62
072400050711     C*
072500050711     C* ...x reperire i campi relativi all' e-mail
072600020923     C                   eval      KNTNK1 = ALFAKCI + %subst(§PJKSC:2:7)
072700050711     C     KEYNTC_P      setll     tfntc01l
072800050711     C                   if        %equal(tfntc01l)
072900050711     C     KEYNTC_P      reade     tfntc01l
073000050711     C                   dow       not %eof(tfntc01l)
073100050711     C     ntcflt        ifne      'A'
073200050711     C                   if        ntctnt  = PARNTE1
073300060619     C                   eval      wSavTNT_01 = ntctnt
073400060619     C                   eval      wSavRNT_01 = ntcrnt
073500060619     C***                eval      wfpeml  = ntcrnt
073600060619     C***                leave
073700050711     C                   else
073800050711     C                   if        ntctnt  = PARNTE2
073900060619     C                   eval      wSavTNT_02 = ntctnt
074000060619     C                   eval      wSavRNT_02 = ntcrnt
074100060619     C***                eval      wfpeml  = ntcrnt
074200060619     C***                leave
074300050711     C                   else
074400050711     C                   if        ntctnt  = PARNTE3
074500060619     C                   eval      wSavTNT_03 = ntctnt
074600060619     C                   eval      wSavRNT_03 = ntcrnt
074700060619     C***                eval      wfpeml  = ntcrnt
074800060619     C***                leave
074900050711     C                   else
075000050711     C                   if        ntctnt  = PARNTE4
075100060619     C                   eval      wSavTNT_04 = ntctnt
075200060619     C                   eval      wSavRNT_04 = ntcrnt
075300060619     C***                eval      wfpeml  = ntcrnt
075400060619     C***                leave
075500050711     C                   else
075600050711     C                   if        ntctnt  = PARNTE5
075700060619     C                   eval      wSavTNT_05 = ntctnt
075800060619     C                   eval      wSavRNT_05 = ntcrnt
075900060619     C***                eval      wfpeml  = ntcrnt
076000060619     C***                leave
076100050711     C                   else
076200050711     C                   if        ntctnt  = PARNTE6
076300060619     C                   eval      wSavTNT_06 = ntctnt
076400060619     C                   eval      wSavRNT_06 = ntcrnt
076500060619     C***                eval      wfpeml  = ntcrnt
076600060619     C***                leave
076700050711     C                   else
076800050711     C                   if        ntctnt  = PARNTE7
076900060619     C                   eval      wSavTNT_07 = ntctnt
077000060619     C                   eval      wSavRNT_07 = ntcrnt
077100060619     C***                eval      wfpeml  = ntcrnt
077200060619     C***                leave
077300050711     C                   else
077400050711     C                   if        ntctnt  = PARNTE8
077500060619     C                   eval      wSavTNT_08 = ntctnt
077600060619     C                   eval      wSavRNT_08 = ntcrnt
077700060619     C***                eval      wfpeml  = ntcrnt
077800060619     C***                leave
077900050711     C                   endif
078000050711     C                   endif
078100050711     C                   endif
078200050711     C                   endif
078300050711     C                   endif
078400050711     C                   endif
078500050711     C                   endif
078600050711     C                   endif
078700050711     C*
078800050711     C                   endif
078900050711     C     KEYNTC_P      reade     tfntc01l
079000050711     C                   enddo
079100050711     C                   endif
079200060619     C*
079300060619     C* Verifico priorità note reperite
079400060619     C                   if        wSavTNT_01 <> *blanks
079500060619     C                   eval      wSavTNT = wSavTNT_01
079600060619     C                   eval      wfpeml  = wSavRNT_01
079700060619     C                   else
079800060619     C                   if        wSavTNT_02 <> *blanks
079900060619     C                   eval      wSavTNT = wSavTNT_02
080000060619     C                   eval      wfpeml  = wSavRNT_02
080100060619     C                   else
080200060619     C                   if        wSavTNT_03 <> *blanks
080300060619     C                   eval      wSavTNT = wSavTNT_03
080400060619     C                   eval      wfpeml  = wSavRNT_03
080500060619     C                   else
080600060619     C                   if        wSavTNT_04 <> *blanks
080700060619     C                   eval      wSavTNT = wSavTNT_04
080800060619     C                   eval      wfpeml  = wSavRNT_04
080900060619     C                   else
081000060619     C                   if        wSavTNT_05 <> *blanks
081100060619     C                   eval      wSavTNT = wSavTNT_05
081200060619     C                   eval      wfpeml  = wSavRNT_05
081300060619     C                   else
081400060619     C                   if        wSavTNT_06 <> *blanks
081500060619     C                   eval      wSavTNT = wSavTNT_06
081600060619     C                   eval      wfpeml  = wSavRNT_06
081700060619     C                   else
081800060619     C                   if        wSavTNT_07 <> *blanks
081900060619     C                   eval      wSavTNT = wSavTNT_07
082000060619     C                   eval      wfpeml  = wSavRNT_07
082100060619     C                   else
082200060619     C                   if        wSavTNT_08 <> *blanks
082300060619     C                   eval      wSavTNT = wSavTNT_08
082400060619     C                   eval      wfpeml  = wSavRNT_08
082500060619     C                   endif
082600060619     C                   endif
082700060619     C                   endif
082800060619     C                   endif
082900060619     C                   endif
083000060619     C                   endif
083100060619     C                   endif
083200060619     C                   endif
083300050711     C*
083400050711     C* Inserisco veirifica x modifica carattere § in @
083500050711     C     '§':'@'       xlate     wfpeml        wfpeml
083600060207     C                   eval      wfptne = wSavTNT
083700050711     C*
083800050711     C* ...x reperire i campi relativi al responsabile SOLO X TIPO NOTA 02 o 05
083900050711     C* ...ovvero se tipo nota e-mail reperito in precedenza è 03 o 06
084000050711     C                   if        wSavTNT = '03'
084100050711     C                   eval      KNTTNT = '02'
084200050711     C     KEYNTC        chain     tfntc01l
084300050711     C                   if        %found(tfntc01l)
084400050711     C     ntcflt        ifne      'A'
084500050711     C                   eval      wfprsp = ntcrnt
084600060207     C                   eval      wfptnr = ntctnt
084700050711     C                   endif
084800050711     C                   endif
084900050711     C                   endif
085000050711     C*
085100050711     C                   if        wSavTNT = '06'
085200050711     C                   eval      KNTTNT = '05'
085300050711     C     KEYNTC        chain     tfntc01l
085400050711     C                   if        %found(tfntc01l)
085500050711     C     ntcflt        ifne      'A'
085600050711     C                   eval      wfprsp = ntcrnt
085700061114     C                   eval      wfptnt = ntctnt
085800050711     C                   endif
085900050711     C                   endif
086000050711     C                   endif
086100050711     C*
086200050711     C*    KEYNTC        setll     tfntc01l
086300050711     C*                  if        %equal(tfntc01l)
086400050711     C*    KEYNTC        reade     tfntc01l
086500050711     C*                  dow       not %eof(tfntc01l)
086600050711     C*    ntcflt        ifne      'A'
086700050711     C*                  if        ntctnt = PARNTN1
086800050711     C*                  eval      wfprsp = ntcrnt
086900050711     C*                  leave
087000050711     C*                  else
087100050711     C*                  if        ntctnt = PARNTN2
087200050711     C*                  eval      wfprsp = ntcrnt
087300050711     C*                  leave
087400050711     C*                  else
087500050711     C*                  if        ntctnt = PARNTN3
087600050711     C*                  eval      wfprsp = ntcrnt
087700050711     C*                  leave
087800050711     C*                  else
087900050711     C*                  if        ntctnt = PARNTN4
088000050711     C*                  eval      wfprsp = ntcrnt
088100050711     C*                  leave
088200050711     C*                  endif
088300050711     C*                  endif
088400050711     C*                  endif
088500050711     C*                  endif
088600050711     C*
088700050711     C*                  endif
088800050711     C*    KEYNTC        reade     tfntc01l
088900050711     C*                  enddo
089000050711     C                   endif
089100000922     C*
089200000922     C                   movel(p)  'X'           wfpchk
089300000922     C*
089400000922     C                   ENDSR
089500011127     C*------------------------------------------------------------------------*
089600011127     C* OVRVALREC - Routine di sovrapposizione valori di valorizzazione buffer del record
089700011127     C*------------------------------------------------------------------------*
089800011127     C     OVRVALREC     BEGSR
089900011127     C*
090000050405     C* Reperisco i dati del soggetto corrente
090100020923     C                   IF        §PJKCC = '00'+ALFAKCI
090200020311     C                   EVAL      wrknatura = 'C'
090300020311     C                   ENDIF
090400050406     C                   IF        §PJKCC = '000400'
090500050406     C                   EVAL      wrknatura = 'F'
090600050406     C                   ENDIF
090700060222     C*
090800060222     C* Se se richiesto espressamente nel lancio sovrapposizione cliente fatturazine a cliente KSC
090900060222     C* ...altrimenti valorizzo coi dati del cliente KSC
091000071228     C                   if        PAROVR = 'F'
091100060222     C                   eval      w§PJCLF = §PJCLF
091200060222     C                   else
091300071228     C                   if        PAROVR = 'U'
091400071228     C                   exsr      RTVPARENT
091500071228     C                   move(p)   §D10COP       w§PJCLF
091600060222     C                   endif
091700071228     C                   endif
091800020311     C*
091900050406     C     KEYRCO98J     CHAIN     ANRCO98J
092000050404     C     KEYCLN        CHAIN     ANCLN01L
092100011127     C*
092200011127     C* Dopo aver reperito i dati eseguo appunto le sovrapposizioni di valori
092300020311     C                   EVAL      §PJDES = SOGDES
092400020311     C                   EVAL      §PJIND = INDINDRIZ
092500020311     C                   EVAL      §PJLOC = INDLOCALIT
092600020311     C                   EVAL      §PJCAP = INDCAP
092700020311     C                   EVAL      §PJPRV = INDPROV
092800050404     C                   IF        wrknatura = 'C'
092900020311     C                   EVAL      §PJABC = CLNCLASABC
093000050404     C                   ENDIF
093100020311     C                   EVAL      §PJTEL = INDTELEFON
093200020311     C                   EVAL      §PJNAZ = INDSTATO
093300020311     C                   EVAL      §PJIVA = SOGPARTIVA
093400020311     C                   EVAL      §PJCDF = SOGCDFISC
093500011127     C*
093600011127     C                   ENDSR
093700000922     C*------------------------------------------------------------------------*
093800000922     C* EXECHK  - Routine di considerazioni finali
093900000922     C*------------------------------------------------------------------------*
094000000922     C     EXECHK        BEGSR
094100060209     C*
094200060209     C*****
094300060209     C* 1° GIRO DI CONTROLLO (nn considero, ergo elimino, i record aventi codice commerciale
094400060209     C*                       "vari" o "inattivi" ovvero quelli con XXX0000 o XXX0999)
094500060209     C*****
094600060209     C*
094700060209     C     *start        setll     wfptl00f
094800060209     C                   dow       not %eof(wfptl00f)
094900060209     C                   read      wfptl00f
095000060209     C                   if        not %eof(wfptl00f)
095100110427     C*
095200110427     C* Solo x i soggetti d tipo cliente ("C")
095300110427     C                   if        wfptip = 'C'
095400071231     C                   move(p)   f_wfpcom      wCom              4
095500060209     C                   if        wCom = '0000' OR wCom = '0999'
095600060209     C                   delete    wfptl00f
095700060209     C                   endif
095800110427     C                   endif
095900060209     C                   endif
096000060209     C                   enddo
096100030306     C*
096200030306     C*****
096300060209     C* 2° GIRO DI CONTROLLO (attribuzione e-mail e responsabile dei soggetti non "battezzati"
096400050405     C*                       come unificanti ai soggetti invece unificanti che
096500050405     C*                       eventualmente ne fossero sprovvisti (naturalmente unificanti
096600060221     C*                       x medesima P.IVA/C.FISC). => solo se richiesto nei parametri
096700030306     C*****
096800030306     C                   eval      depioc = *loval
096900060221     C*
097000060221     C                   if        PARRGR = 'S'
097100030306     C*
097200030306     C* Apro il file (WFPTL05L)
097300030306     C                   open      wfptl05l
097400030306     C*
097500030306     C     *loval        setll     wfptl05l
097600030306     C                   dow       not %eof(wfptl05l)
097700030306     C                   read      wfptl05l
097800030306     C                   if        not %eof(wfptl05l)
097900030306     C                   if        wfpioc <> depioc
098000030306     C                   eval      depioc = wfpioc
098100030306     C                   eval      depeml = *blanks
098200030306     C                   eval      deprsp = *blanks
098300030306     C                   if        wfpeml <> *blanks
098400030306     C                   eval      depeml = wfpeml
098500030306     C                   endif
098600030306     C                   if        wfprsp <> *blanks
098700030306     C                   eval      deprsp = wfprsp
098800030306     C                   endif
098900030306     C                   else
099000030306     C                   if        wfpeml <> *blanks
099100030306     C                   eval      depeml = wfpeml
099200030306     C                   endif
099300030306     C                   if        wfprsp <> *blanks
099400030306     C                   eval      deprsp = wfprsp
099500030306     C                   endif
099600030306     C                   if        (wfpeml = *blanks  OR
099700030306     C                              wfprsp = *blanks) AND
099800030306     C                             (depeml <> *blanks OR
099900030306     C                              deprsp <> *blanks)
100000030306     C                   if        wfpeml = *blanks
100100030306     C                   eval      wfpeml = depeml
100200030306     C                   endif
100300030306     C                   if        wfprsp = *blanks
100400030306     C                   eval      wfprsp = deprsp
100500030306     C                   endif
100600030306     C                   update    wfptl005
100700030306     C                   endif
100800030306     C                   endif
100900030306     C                   endif
101000030306     C                   enddo
101100030306     C*
101200030306     C* Chiudo il file (WFPTL05L)
101300030306     C                   close     wfptl05l
101400060221     C                   endif
101500011127     C*
101600011127     C*****
101700071228     C* 3° GIRO DI CONTROLLO (raggruppamento record x medesima P.IVA/COD.FISC. con sommatoria
101800050406     C*                       dei fatturati)
101900011127     C*****
102000011127     C                   eval      depioc = *loval
102100000922     C*
102200071231     C* Apro i file (WFPTL02L)
102300000922     C                   open      wfptl02l
102400000922     C*
102500000922     C     *loval        setll     wfptl02l
102600000922     C                   dow       not %eof(wfptl02l)
102700071231     C                   read(n)   wfptl02l
102800011127     C                   if        not %eof(wfptl02l)
102900050406     C                   exsr      decodifatt
103000011127     C                   endif
103100000922     C                   enddo
103200050406     C*
103300050406     C* Dopo l'ultima lettura eseguo nuovamente il test x verificare
103400050406     C* se occorre eseguire la sommatoria del fatturato dell'ultimo record sempre
103500050406     C* x medesima P.IVA/COD.FISC.
103600071231     C                   exsr      EXESUMFATT
103700000922     C*
103800071231     C* Chiudo i file (WFPTL02L)
103900000922     C                   close     wfptl02l
104000071212     C*
104100050406     C*****
104200071228     C* 4° GIRO DI CONTROLLO (eliminazione record risultanti che dalle precedenti elaborazioni
104300050406     C*                       risultano nn conformi ai parametri di lancio)
104400050406     C*****
104500050406     C                   eval      depioc = *loval
104600050406     C*
104700050406     C* Apro il file (WFPTL02L)
104800050406     C                   open      wfptl02l
104900050406     C*
105000050406     C     *loval        setll     wfptl02l
105100050406     C                   dow       not %eof(wfptl02l)
105200050406     C                   read      wfptl02l
105300050406     C                   if        not %eof(wfptl02l)
105400050406     C                   exsr      decodi
105500050406     C                   endif
105600050406     C                   enddo
105700050406     C*
105800050406     C* Chiudo il file (WFPTL02L)
105900050406     C                   close     wfptl02l
106000011127     C*
106100011127     C*****
106200071228     C* 5° GIRO DI CONTROLLO (elimino record in modo da tenere x medesima P.IVA/C.FISC solo soggetto
106300030306     C*                       con maggior fatturato)
106400011127     C*****
106500011127     C                   eval      depksc = *loval
106600050404     C                   eval      depsoc = *loval
106700011127     C*
106800011127     C* Apro il file (WFPTL04L)
106900011127     C                   open      wfptl04l
107000011127     C*
107100011127     C     *loval        setll     wfptl04l
107200011127     C                   dow       not %eof(wfptl04l)
107300011127     C                   read      wfptl04l
107400011127     C                   if        not %eof(wfptl04l)
107500011127     C*
107600050404     C* Controllo sulla P.IVA ==> x medesima (vuota) rompo anche per società/codice soggetto
107700011127     C* e tengo buona quello avente maggior fatturato
107800050404     C                   if        wfpsoc = depsoc AND                          considerazioni
107900050404     C                             wfpksc = depksc                              considerazioni
108000011127     C                   delete    wfptl004
108100011127     C                   else
108200050404     C                   movel(p)  wfpsoc        depsoc
108300050404     C                   movel(p)  wfpksc        depksc
108400011127     C                   endif
108500011127     C                   endif
108600011127     C*
108700011127     C                   enddo
108800011127     C*
108900011127     C* Chiudo il file (WFPTL04L)
109000011127     C                   close     wfptl04l
109100060221     C*
109200000922     C                   ENDSR
109300011127     C*------------------------------------------------------------------------*
109400050405     C* EXEAGE - Routine di reperimento agente commerciale legato al cliente
109500011127     C*------------------------------------------------------------------------*
109600011127     C     EXEAGE        BEGSR
109700011127     C*
109800011127     C                   movel     §PJSOC        KBASOC
109900020923     C                   movel     *ALL'0'       KBAKCC
110000020923     C                   move      DUTKCI        KBAKCC
110100011127     C                   move      wfpksc        KBAKSC
110200011127     C     KEYYBA        chain     ybacl01l
110300011127     C                   if        %found(ybacl01l)
110400011127     C                   z-add     aclage        wfpcom
110500130829     C* Reperisco il nome ("01") e l'e-mail ("02") del commerciale
110600130829     C                   eval      NTCcmm = WFPcom
110700130829     C                   eval      NTCtnt = '01'
110800130829     C     k_AZNTC01     chain     AZNTC000
110900130829     C                   if        %found(AZNTC01L)
111000130829     C                   movel     NTCrnt        wfpdec
111100130829     C                   endif
111200130829     C                   eval      NTCtnt = '02'
111300130829     C     k_AZNTC01     chain     AZNTC000
111400130829     C                   if        %found(AZNTC01L)
111500130829     C                   movel     NTCrnt        wfpemc
111600130829     C                   endif
111700011127     C                   endif
111800011127     C*
111900011127     C                   ENDSR
112000081205     C*------------------------------------------------------------------------*
112100081205     C* EXETBC - Routine di reperimento codice blocco cliente
112200081205     C*------------------------------------------------------------------------*
112300081205     C     EXETBC        BEGSR
112400081205     C*
112500081205     C                   movel     §PJSOC        KBASOC
112600081205     C                   movel     *ALL'0'       KBAKCC
112700081205     C                   move      DUTKCI        KBAKCC
112800081205     C                   movel     §PJKSC        KBAKSC
112900081205     C     KEYYBA        chain     ybacl01l
113000081205     C                   if        %found(ybacl01l)
113100081205     C                   if        acltbc <> *blanks
113200081205     C                   movel     acltbc        wfptbc
113300081205     C                   endif
113400081205     C                   endif
113500081205     C*
113600081205     C                   ENDSR
113700000928     C*------------------------------------------------------------------------*
113800000928     C* CHKREC - Routine di controllo validità del record attivo dello snapshot
113900000928     C*------------------------------------------------------------------------*
114000000928     C     CHKREC        BEGSR
114100000928     C*
114200000928     C                   movel     '0'           errl00            1
114300000928     C                   movel     *blanks       flgblk            1
114400001211     C*
114500001211     C* Verifico, qualora richiesto, che il cliente sia del P.O. indicato
114600050405     C                   if        PARFIL > *zeros AND PARCOF = 'C'
114700001211     C                   if        errl00 = '0'
114800001211     C                   movel     §PJKSC        alfksc            4
114900001211     C                   move      alfksc        numksc            3 0
115000001211     C                   if        numksc <> PARFIL
115100001211     C                   movel     '1'           errl00
115200001211     C                   endif
115300001211     C                   endif
115400001211     C                   endif
115500000928     C*
115600000928     C* Controllo che il cliente sia o non sia bloccato (a seconda dei parametri di lancio)
115700001211     C                   if        PARCOF = 'C'
115800000928     C                   if        errl00 = '0'
115900000928     C                   movel     §PJSOC        KBASOC
116000020923     C                   movel     *ALL'0'       KBAKCC
116100020923     C                   move      DUTKCI        KBAKCC
116200000928     C                   movel     §PJKSC        KBAKSC
116300000928     C     KEYYBA        chain     ybacl01l
116400000928     C                   if        %found(ybacl01l)
116500000928     C                   if        acltbc <> *blanks
116600060220     C***                movel     *hival        flgblk
116700060220     C                   movel     acltbc        flgblk
116800000928     C                   endif
116900000928     C                   endif
117000000928     C*
117100000928     C* Considerazioni finali
117200000928     C                   if        PARBLK = 'B' and flgblk =  *blanks
117300000928     C                   movel     '1'           errl00
117400000928     C                   endif
117500000928     C                   if        PARBLK = 'N' and flgblk <> *blanks
117600000928     C                   movel     '1'           errl00
117700000928     C                   endif
117800000928     C                   endif
117900001211     C                   endif
118000000928     C*
118100000928     C                   ENDSR
118200011016     C*------------------------------------------------------------------------*
118300011016     C* EXEINDUNIQ - Routine di eliminazione indirizzi doppi
118400011016     C*------------------------------------------------------------------------*
118500011016     C     EXEINDUNIQ    BEGSR
118600011016     C*
118700011016     C* Se richiesto elimino gli indirizzi doppi
118800011016     C                   if        PARIND = 'S'
118900011016     C*
119000011016     C* Apro il file (WFPTL03L)
119100011016     C                   open      wfptl03l
119200011016     C*
119300011016     C* Inizializzo i campi di deposito
119400011016     C                   eval      wrkrag = *loval
119500011016     C                   eval      wrkind = *loval
119600011016     C                   eval      wrkloc = *loval
119700011016     C                   eval      wrkpro = *loval
119800011016     C                   eval      wrkcap = *loval
119900011016     C                   eval      wrknaz = *loval
120000011016     C                   eval      countInd = *zeros
120100011016     C*
120200011016     C     *loval        setll     wfptl03l
120300011016     C                   read(e)   wfptl03l                                     1° lett. fuori ciclo
120400011016     C                   eval      countInd = countInd + 1
120500011016     C                   dow       not %eof(wfptl03l)
120600011016     C                   if        wfprag = wrkrag and
120700011016     C                             wfpind = wrkind and
120800011016     C                             wfploc = wrkloc and
120900011016     C                             wfppro = wrkpro and
121000011016     C                             wfpcap = wrkcap and
121100011016     C                             wfpnaz = wrknaz
121200011016     C                   if        countInd > *zeros
121300011016     C                   delete    wfptl03l
121400011016     C                   endif
121500011016     C                   else
121600011016     C* Riassegno i campi di deposito
121700011016     C                   eval      wrkrag = wfprag
121800011016     C                   eval      wrkind = wfpind
121900011016     C                   eval      wrkloc = wfploc
122000011016     C                   eval      wrkpro = wfppro
122100011016     C                   eval      wrkcap = wfpcap
122200011016     C                   eval      wrknaz = wfpnaz
122300011016     C                   eval      countInd = *zeros
122400011016     C                   endif
122500011016     C                   read(e)   wfptl03l                                     letture dentro ciclo
122600011016     C                   eval      countInd = countInd + 1
122700011016     C                   enddo
122800011016     C*
122900011016     C* Chiudo il file (WFPTL03L)
123000011016     C                   close     wfptl03l
123100011016     C*
123200011016     C                   endif
123300011016     C*
123400011016     C                   ENDSR
123500011127     C*------------------------------------------------------------------------*
123600011127     C* EXEDEL888999 - Routine di eliminazione codici 8888/9999
123700011127     C*------------------------------------------------------------------------*
123800011127     C     EXEDEL888999  BEGSR
123900011127     C*
124000071231     C     *start        setll     wfptl00f
124100011127     C                   read      wfptl00f                                     1° lett. fuori ciclo
124200011127     C                   dow       not %eof(wfptl00f)
124300071231     C                   move      f_wfpksc      wrkcli            4 0
124400011127     C                   if        wrkcli = 8888 or
124500011127     C                             wrkcli = 9999 or
124600071231     C                             %scan('ATTRIBUIRE':f_wfprag) > *zeros or
124700071231     C                             %scan('TARIFFA':f_wfprag) > *zeros
124800011127     C                   delete    wfptl00f
124900011127     C                   endif
125000011127     C                   read      wfptl00f                                     letture dentro ciclo
125100011127     C                   enddo
125200011127     C*
125300011127     C                   ENDSR
125400000925     C*------------------------------------------------------------------------*
125500000925     C* DECODI - Routine di effettuazione controlli di validità di contenuto
125600000925     C*------------------------------------------------------------------------*
125700000925     C     DECODI        BEGSR
125800000925     C*
125900000925     C                   movel     '0'           errl00            1
126000000925     C*
126100001128     C* Controllo che il cliente sia effettivamente di una "vera" Filiale o Agenzia solo
126200001128     C* se la società È Bartolini o SDI
126300001207     C                   if        errl00 = '0'
126400050405     C                   if        PARCOF = 'C'
126500050404     C                   if        wfpsoc = '201' or
126600050404     C                             wfpsoc = '101'
126700050405     C                   if        PARFOA = 'S'
126800001128     C                   movel(p)  wfpksc        clifil            3 0
126900001128     C     clifil        lookup    FILAGE                                 20
127000001128     C                   if        *in20  = '0' or clifil = *zeros
127100001128     C                   movel     '1'           errl00
127200001128     C                   endif
127300001128     C                   endif
127400001128     C                   endif
127500050405     C                   endif
127600001128     C                   endif
127700001211     C*
127800001211     C* Controllo che, se richiesta per 'F' (fornitori) si tratti di un fornitore effettivo
127900001211     C                   if        errl00 = '0'
128000001211     C                   if        PARCOF = 'F'
128100001211     C                   if        %subst(wfprag:1:1) = '$'
128200001211     C                   movel     '1'           errl00
128300001211     C                   endif
128400001211     C                   endif
128500001211     C                   endif
128600000925     C*
128700000925     C* Controllo che il fatturato del cliente sia conforme ai parametri di lancio
128800000925     C                   if        errl00 = '0'
128900050406     C                   if        PARFAT <> *zeros
129000060615     C                   if        wfpfat < PARFAT and
129100061114     C                             wfpson <> 'S'
129200000925     C                   movel     '1'           errl00
129300000925     C                   endif
129400050406     C                   endif
129500001211     C                   endif
129600000925     C*
129700000925     C* Controllo sulla P.IVA/COD.FISC ==> se entrambi a *blanks non considero il cliente
129800000925     C                   if        errl00 = '0'
129900001207     C                   if        PARCPI = 'N' and
130000060220     C                             (wfpioc = *blanks OR
130100060220     C                              %trim(wfpioc) = '$$')
130200000925     C                   movel     '1'           errl00
130300000925     C                   endif
130400000925     C                   endif
130500060220     C*
130600060220     C* Controllo sul flag di codice blocco cliente => se <> *blanks e richiesto nel lancio
130700060220     C* nn considero il cliente
130800060220     C                   if        errl00 = '0'
130900060222     C                   if        WFPTBC <> *blanks AND
131000060220     C                             partbc = 'S'
131100060220     C                   movel     '1'           errl00
131200060220     C                   endif
131300060220     C                   endif
131400000925     C*
131500000925     C* Controllo sul flag di check ==> se diverso da 'X' non considero
131600000925     C                   if        errl00 = '0'
131700060222     C                   if        wfpchk <> 'X' and
131800060222     C                             parchk =  'S'
131900000925     C                   movel     '1'           errl00
132000000925     C                   endif
132100000925     C                   endif
132200000925     C*
132300000925     C                   if        errl00 = '1'
132400000925     C                   delete    wfptl002
132500000925     C                   endif
132600000925     C*
132700000925     C                   ENDSR
132800060222     C*------------------------------------------------------------------------*
132900060222     C* DECODIFATT - Routine di effettuazione raggruppamento P.IVA/COD.FISC. e somma fatturato
133000060222     C*------------------------------------------------------------------------*
133100060222     C     DECODIFATT    BEGSR
133200060222     C*
133300060222     C                   if        wfpioc <> *blanks
133400060222     C                   if        wfpioc = depioc                              considerazioni
133500060222     C* Salvo l'ammontare del record corrente (che verrà eliminato) x portarlo in somma a quello
133600060222     C* del record che rimarrà (ovviamente x medesima P.IVA/COD.FISC.)
133700071231     C                   add       wfpfat        depfat
133800071231     C     wfptlnrri     chain     wfptl00f
133900071231     C                   if        %found(wfptl00f)
134000071231     C                   delete    wfptlfff
134100071231     C                   endif
134200071231     C*
134300060222     C                   else
134400071231     C* A cambio P.IVA/COD.FISC. aggiorno il fatturato sempre sul primo record
134500071231     C                   exsr      EXESUMFATT
134600060222     C                   endif
134700060222     C                   endif
134800060222     C*
134900060222     C                   ENDSR
135000071231     C*------------------------------------------------------------------------*
135100071231     C* EXESUMFATT - Routine di sommatoria e aggiornamento fatturato
135200071231     C*------------------------------------------------------------------------*
135300071231     C     EXESUMFATT    BEGSR
135400071231     C*
135500071231     C                   if        depioc <> *loval
135600071231     C     wfptlnrrp     chain     wfptl00f
135700071231     C                   if        %found(wfptl00f)
135800071231     C                   eval      f_wfpfat = depfat
135900071231     C                   update    wfptlfff
136000071231     C                   endif
136100071231     C                   endif
136200071231     C*
136300071231     C                   z-add     wfpfat        depfat
136400071231     C                   movel(p)  wfpioc        depioc
136500071231     C                   eval      wfptlnrrp = wfptlnrri
136600071231     C*
136700071231     C                   ENDSR
136800001120     C*------------------------------------------------------------------------*
136900001120     C* EXEANAORG  - Routine di attribuzione classificazione territoriale clienti (DIV/AREA/FIL)
137000001120     C*------------------------------------------------------------------------*
137100001120     C     EXEANAORG     BEGSR
137200001120     C*
137300001120     C* Apro il file (WFPTL02L)
137400001120     C                   open      wfptl02l
137500001120     C*
137600001120     C     *loval        setll     wfptl02l
137700001120     C                   read(e)   wfptl02l
137800001120     C                   dow       not %eof(wfptl02l)
137900050404     C*
138000050404     C* Se cliente
138100050404     C                   if        (PARCOF = 'C' and
138200050404     C                             (wfpsoc = '201' or wfpsoc = '101'))
138300001120     C* Estrapolo la filiale del cliente corrente
138400001120     C                   movel     wfpksc        depfil            3 0
138500001120     C     depfil        chain     azorg01l
138600020220     C                   if        %found(azorg01l)
138700001120     C                   z-add     orgfil        wfpfil
138800001120     C                   z-add     orgcar        wfpare
138900001120     C                   movel     orgfl3        wfpdiv
139000020220     C                   movel     orgdes        wfpdes
139100020220     C                   movel     orgind        wfpinf
139200020220     C                   z-add     orgcpf        wfpcpf
139300020220     C                   movel     orgloc        wfplof
139400020220     C                   movel     orgpro        wfpprf
139500020220     C                   movel     orgtel        wfptef
139600020220     C                   movel     orgfax        wfpfaf
139700001120     C                   update    wfptl002
139800001120     C                   endif
139900050404     C                   endif
140000050404     C*
140100050404     C* Se fornitore reperisco struttura commerciale tramite CAP/LOCALITA con TISI95R
140200050404     C                   if        PARCOF = 'F'
140300050404     C                   clear                   TISI95DS
140400050404     C                   eval      I95TCN = '3'
140500050404     C                   eval      I95CAP = wfpCAP
140600050404     C                   eval      I95PRV = wfpPRO
140700050404     C                   eval      I95LOC = wfpLOC
140800050404     C                   eval      I95NAR = wfpNAZ
140900050404     C                   call      'TISI95R'
141000050404     C                   parm                    TISI95DS
141100050404     C                   if        O95ERR = *blanks
141200050404     C                   z-add     O95LNA        depfil
141300050404     C     depfil        chain     azorg01l
141400050404     C                   if        %found(azorg01l)
141500050404     C                   z-add     orgfil        wfpfil
141600050404     C                   z-add     orgcar        wfpare
141700050404     C                   movel     orgfl3        wfpdiv
141800050404     C                   movel     orgdes        wfpdes
141900050404     C                   movel     orgind        wfpinf
142000050404     C                   z-add     orgcpf        wfpcpf
142100050404     C                   movel     orgloc        wfplof
142200050404     C                   movel     orgpro        wfpprf
142300050404     C                   movel     orgtel        wfptef
142400050404     C                   movel     orgfax        wfpfaf
142500050404     C                   update    wfptl002
142600050404     C                   endif
142700050404     C                   endif
142800050404     C                   endif
142900050404     C*
143000001120     C                   read(e)   wfptl02l
143100001120     C                   enddo
143200001120     C*
143300001120     C* Chiudo il file (WFPTL02L)
143400001120     C                   close     wfptl02l
143500001120     C*
143600001120     C                   ENDSR
143700000925     C*------------------------------------------------------------------------*
143800000925     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
143900000925     C*------------------------------------------------------------------------*
144000000925     C     CARORG        BEGSR
144100000925     C*
144200000925     C*---
144300000925     C* FILIALI O AGENZIE
144400000925     C*---
144500000925     C                   z-add     1             idx
144600000925     C*
144700000925     C     *loval        setll(e)  azorg01l
144800000925     C                   read(e)   azorg01l
144900000925     C                   dow       not(%eof)
145000000925     C                   if        (orgfag='A' or orgfag='F') and orgfva=*blanks
145100000925     C                   z-add     orgfil        FILAGE(idx)
145200000925     C                   add       1             idx
145300000925     C                   endif
145400000925     C                   read(e)   azorg01l
145500000925     C                   enddo
145600000925     C*
145700000925     C                   ENDSR
145800020923     C*--------------------------------------------------------------------------------------------*
145900020923     C* REPERISCE I DATI UTENTE
146000020923     C*--------------------------------------------------------------------------------------------*
146100020923     C     REPDATIUTE    BEGSR
146200020923     C*
146300020923     C* INIZIALIZZA VARIABILI DI WRK
146400020923     C                   CLEAR                   TIBS34DS
146500020923     C                   CLEAR                   AZUTEDS
146600020923     C                   CLEAR                   DDATIUTE
146700020923     C*
146800020923     C     *DTAARA       DEFINE    §azute        azuteds
146900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
147000020923     C                   IN(E)     *DTAARA
147100020923if  1C                   IF        %Error
147200020923     c                   EVAL      I34Tla = 'L'
147300020923     C                   CALL      'TIBS34R'
147400020923     C                   PARM                    Tibs34Ds
147500020923     C                   IN        *DTAARA
147600020923e   1C                   ENDIF
147700020923     C*
147800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
147900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
148000020923     C*
148100020923     C                   ENDSR
148200000000     C*------------------------------------------------------------------------*
148300000000     C* *INZSR - ROUTINE INIZIALE
148400000000     C*------------------------------------------------------------------------*
148500000000     C     *INZSR        BEGSR
148600000000     C*--------------------
148700000000     C* RICEVIMENTO PARAMETRI
148800000000     C*--------------------
148900000000     C     *ENTRY        PLIST
149000000000     C                   PARM                    KPJBA
149100000000     C                   MOVEL     KPJBU         PARAM
149200020923     C*
149300020923     C* REPERISCE I DATI DELL'UTENTE
149400020923     C                   EXSR      REPDATIUTE
149500020923     C                   MOVEL     DUTKCI        ALFAKCI           4
149600000000     C*--------------------
149700000000     C* CAMPI RIFERITI AL DATA BASE
149800000000     C*--------------------
149900050404     C     *LIKE         DEFINE    WFPSOC        KWPSOC
150000050404     C     *LIKE         DEFINE    WFPKSC        KWPKSC
150100000922     C     *LIKE         DEFINE    NTCAPL        KNTAPL
150200000922     C     *LIKE         DEFINE    NTCNK1        KNTNK1
150300000922     C     *LIKE         DEFINE    NTCNK2        KNTNK2
150400000922     C     *LIKE         DEFINE    NTCTNT        KNTTNT
150500000928     C     *LIKE         DEFINE    ACLSOCIETA    KBASOC
150600000928     C     *LIKE         DEFINE    ACLKCC        KBAKCC
150700000928     C     *LIKE         DEFINE    ACLKSC        KBAKSC
150800000922     C*
150900000922     C                   MOVEL(P)  'C'           KNTAPL
151000000922     C*
151100000000     C*--------------------
151200000000     C* CHIAVI DI LETTURA
151300000000     C*--------------------
151400050404     C*
151500050404     C* CHIAVE LETTURA WFPTL01L - COMPLETA
151600050404     C     KEYPTL01      KLIST
151700050404     C                   KFLD                    KWPSOC
151800050404     C                   KFLD                    KWPKSC
151900000922     C*
152000000922     C* CHIAVE LETTURA TFNTC01L - COMPLETA
152100050711     C     KEYNTC_P      KLIST
152200000922     C                   KFLD                    KNTAPL
152300000922     C                   KFLD                    KNTNK1
152400000922     C                   KFLD                    KNTNK2
152500050711     C*
152600050711     C* CHIAVE LETTURA TFNTC01L - COMPLETA
152700050711     C     KEYNTC        KLIST
152800050711     C                   KFLD                    KNTAPL
152900050711     C                   KFLD                    KNTNK1
153000050711     C                   KFLD                    KNTNK2
153100050711     C                   KFLD                    KNTTNT
153200000928     C*
153300000928     C* CHIAVE LETTURA YBACL01L - COMPLETA
153400000928     C     KEYYBA        KLIST
153500000928     C                   KFLD                    KBASOC
153600000928     C                   KFLD                    KBAKCC
153700000928     C                   KFLD                    KBAKSC
153800060202     C*
153900130829     C* CHIAVE LETTURA AZNTC01L - COMPLETA
154000130829     C     K_AZNTC01     KLIST
154100130829     C                   KFLD                    NTCCMM
154200130829     C                   KFLD                    NTCTNT
154300020311     C*
154400020311     C* CHIAVE LETTURA ANRCO98J - COMPLETA
154500050406     C     KEYRCO98J     KLIST
154600020311     C                   KFLD                    §PJSOC
154700020311     C                   KFLD                    wrknatura
154800040519     C***                KFLD                    §PJKSC
154900060222     C                   KFLD                    w§PJCLF
155000050406     C*
155100050406     C* CHIAVE LETTURA ANRCO01L - COMPLETA
155200050406     C     KEYRCO01L     KLIST
155300050406     C                   KFLD                    §PJSOC
155400050406     C                   KFLD                    §PJKCC
155500050406     C***                KFLD                    §PJKSC
155600060222     C                   KFLD                    §PJCLF
155700020311     C*
155800020311     C* CHIAVE LETTURA ANCLN01L - COMPLETA
155900020311     C     KEYCLN        KLIST
156000020311     C                   KFLD                    §PJSOC
156100040519     C***                KFLD                    §PJKSC
156200060222     C                   KFLD                    w§PJCLF
156300000921     C*--------------------
156400000921     C* CALCOLA LA DATA CORRENTE
156500000921     C*--------------------
156600000921     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
156700000921     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
156800000921     C                   Z-ADD     WN8           G08DAT
156900000921     C                   Z-ADD     *ZEROS        G08INV
157000000921     C                   MOVEL     '0'           G08ERR
157100000921     C                   CALL      'XSRDA8'
157200000921     C                   PARM                    WLBDA8
157300000921     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
157400000925     C*
157500000925     C* REPERIMENTO DELLE SOLE FILIALI O AGENZIE
157600000925     C                   EXSR      CARORG
157700000000     C*
157800000000     C                   ENDSR
157900000914
158000000920** CMD - COMANDI CL
158100000922CLRPFM FILE(WFPTL00F)
