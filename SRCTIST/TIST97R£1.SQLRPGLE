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
002300020925     FTFNTC01L  IF   E           K DISK
002400020311     FANRCO98J  IF   E           K DISK
002500050406     FANRCO01L  IF   E           K DISK    rename(ANRCO000:ANRCO001) prefix(F_)
002600020311     FANCLN01L  IF   E           K DISK
002700020220     FTABEL00F  IF   E           K DISK
002800060202     FTNTBE01L  IF   E           K DISK
002900000000     D*------------------------------------------------------------------------*
003000000000     D* SCHIERE
003100000000     D*------------------------------------------------------------------------*
003200000925     D*------------
003300000925     D* FILIALI "VALIDE"
003400000925     D*------------
003500000925     D FILAGE          s              3  0 DIM(300)                             * FILIALI/AGENZIE
003600000920     D*------------
003700000920     D* COMANDI
003800000920     D*------------
003900040902     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
004000000000     D*------------------------------------------------------------------------*
004100000000     D* INPUT
004200000000     D*------------------------------------------------------------------------*
004300000000     D*-------------------
004400000000     D* ARCHITETTURA
004500000000     D*-------------------
004600000000     D KPJBA         E DS
004700020220     D*-------------------
004800060202     D* DS X LETTURA TABELLA "01" => COMMERCIALI
004900020220     D*-------------------
005000020220     D DS01          E DS
005100060202     D*-------------------
005200060202     D* DS X LETTURA TABELLA "HTM" => E-MAIL COMMERCIALI
005300060202     D*-------------------
005400060202     D DHTM          E DS
005500050404     D*-------------------
005600050404     D* DS X REPERIMENTO DATI INTRADAMENTO TRAMITE TISI95R
005700050404     D*-------------------
005800050404     D TISI95DS      E DS
005900000000     D*-------------------
006000000922     D* DS SCOMPOSIZIONE DATE INIZ/FIN
006100000922     D*-------------------
006200000922     D                 DS                  INZ
006300000928     D  DIANN                  1      4
006400000928     D  DIMES                  5      6
006500000928     D  DIMGS                  5      8
006600060717     D  DIDAT6                 1      6
006700000928     D  DIDAT                  1      8
006800000922     D                 DS                  INZ
006900000928     D  DFANN                  1      4
007000000928     D  DFMES                  5      6
007100000928     D  DFMGS                  5      8
007200060717     D  DFDAT6                 1      6
007300000928     D  DFDAT                  1      8
007400000000     D*-------------------
007500000000     D* DS PARAMETRI DI LANCIO PGM
007600000000     D*-------------------
007700060714     D PARAM           DS
007800060714     D  PARDAI                        8  0
007900060714     D  PARDAF                        8  0
008000060714     D  PARFAT                       17  0
008100060714     D  PARKSC                        7
008200060714     D  PARFIL                        3  0
008300060714     D  PARNAZ                        3
008400060714     D  PARCAP                        9
008500060714     D  PARPRV                        2
008600060714     D  PARBLK                        1
008700060714     D  PARCIC                        1
008800060714     D  PARBAR                        1
008900060714     D  PARSDI                        1
009000060714     D  PARPIC                        1
009100060714     D  PARFOA                        1
009200060714     D  PARNTN1                       2
009300060714     D  PARNTE1                       2
009400060714     D  PARSOC                        3
009500060714     D  PARCOF                        1
009600060714     D  PARCPI                        1
009700060714     D  PARIND                        1
009800060714     D  PARNTN2                       2
009900060714     D  PARNTN3                       2
010000060714     D  PARNTN4                       2
010100060714     D  PARNTE2                       2
010200060714     D  PARNTE3                       2
010300060714     D  PARNTE4                       2
010400060714     D  PARNTE5                       2
010500060714     D  PARNTE6                       2
010600060714     D  PARNTE7                       2
010700060714     D  PARNTE8                       2
010800060714     D  PARTBC                        1
010900060714     D  PARRGR                        1
011000060714     D  PAROVR                        1
011100060714     D  PARCHK                        1
011200060714     D  PARSOGN                       1
011300000914     D*-------------------
011400000922     D* DS DI LETTURA RECORDSET DEI SALDI VIA SQL
011500000914     D*-------------------
011600000922     D DSSALDI         DS
011700000922     D  §SDOKSC                1      7  0
011800000922     D  §SDOFAT                8     22  3
011900050404     D*-------------------
012000050404     D* DS DI LETTURA RECORDSET DEL FATTURATO FORNITORI VIA SQL
012100050404     D*-------------------
012200050404     D DSSALDIFORN     DS
012300050404     D  §IVASOCIETA                   3
012400050404     D  §IVAKCCCP                     6
012500050404     D  §IVAKSCCP                     8
012600050404     D  §IVAFATTURATO                11  2
012700050404     D  §IVADATAULTFT                10D
012800050404     D  §IVANOTAACCR                 11  2
012900050404     D  §IVADATAULTNA                10D
013000000922     D*-------------------
013100050405     D* DS DI LETTURA RECORDSET DELLE ANAGRAFICHE SOGGETTI (PRO-J) VIA SQL
013200000922     D*-------------------
013300000922     D DSPJANA         DS
013400000922     D  §PJSOC                 1      3
013500000922     D  §PJSOG                 4     11
013600000922     D  §PJKSC                12     19
013700000922     D  §PJDES                20     63
013800000922     D  §PJIVA                64     83
013900000922     D  §PJCDF                84    103
014000000922     D  §PJIND               104    147
014100000922     D  §PJLOC               148    181
014200000922     D  §PJCAP               182    190
014300000922     D  §PJPRV               191    192
014400000922     D  §PJNAZ               193    196
014500000925     D  §PJAVA               197    199
014600000925     D  §PJCLF               200    207
014700001110     D  §PJABC               208    208
014800001114     D  §PJTEL               209    228
014900020311     D  §PJKCC               229    234
015000060615     D  §PJDTINVL            235    244
015100010914     D*-------------------
015200010914     D* DS DI RIDEFINIZIONE KSC DI PRO-J
015300010914     D*-------------------
015400010914     D DSPJKSC         DS
015500010914     D  WPJFL1                        1
015600010914     D  WPJFIL                        3  0
015700010914     D  WPJFL2                        4
015800071228     D*------------------
015900071228     D* DS REPERIMENTO CLIENTI UNIFICANTI/STAND-ALONE
016000071228     D*-------------------
016100071228     D TIBS10DS      E DS
016200071231     D*------------------
016300071231     D* DS INFO FILE WFPTL02L
016400071231     D*-------------------
016500071231     D wfptlinf        ds
016600071231     D  wfptlnrri            397    400i 0
016700071231     D wfptlnrrp       s             10s 0
016800000914     D*------------------------------------------------------------------------*
016900000918     D* VARIABILI DI WRK
017000000914     D*------------------------------------------------------------------------*
017100000922     D  SQLline        s           5000    varying
017200001211     D  SQLline2       s           5000    varying
017300000922     D  SQLcond        s           5000    varying
017400001211     D  SQLcond2       s           5000    varying
017500000922     D  SQLord         s           5000    varying
017600000926     D  SQLvar         s           5000    varying
017700060529     D  depemlr        s                   like(wfpeml) inz(*loval)
017800000922     D  depioc         s                   like(wfpioc) inz(*loval)
017900011127     D  depksc         s                   like(wfpksc) inz(*loval)
018000050404     D  depsoc         s                   like(wfpsoc) inz(*loval)
018100030306     D  depeml         s                   like(wfpeml) inz(*blanks)
018200030306     D  deprsp         s                   like(wfprsp) inz(*blanks)
018300050406     D  depfat         s                   like(wfpfat) inz(*zeros)
018400010131     D  idx            s              5i 0 inz(*zeros)
018500010131     D  Trovato        s              1
018600011016     D  countInd       s             10  0 inz(*zeros)
018700011016     D  wrkrag         s                   like(wfprag)
018800011016     D  wrkind         s                   like(wfpind)
018900011016     D  wrkloc         s                   like(wfploc)
019000011016     D  wrkpro         s                   like(wfppro)
019100011016     D  wrkcap         s                   like(wfpcap)
019200011016     D  wrknaz         s                   like(wfpnaz)
019300050404     D  wrknatura      s                   like(rcosnatura)
019400050404     D  didatforn      s             10D
019500050404     D  dfdatforn      s             10D
019600060222     D  w§PJCLF        s                   like(§PJCLF) inz(*zeros)
019700060616     D  findata        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
019800000922     D*------------------
019900000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
020000000921     D*------------------
020100000921     D WLBDA8          DS                  INZ
020200000921     D  G08DAT                 1      8  0
020300000921     D  G08INV                 9     16  0
020400000921     D  G08ERR                17     17
020500000921     D  G08TGI                18     22  0
020600020923     D*-------------------
020700020923     D* DS REPERIMENTO DATI UTENTE
020800020923     D*-------------------
020900020923     D TIBS34DS      E DS                                                       *Profili utente
021000020923     D DDATIUTE      E DS                                                       *Dati utente
021100020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
021200000000     C*------------------------------------------------------------------------*
021300000000     C* MAIN LINE
021400000000     C*------------------------------------------------------------------------*
021500071212     C*
021600071212     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
021700071212     C
021800071212     C/EXEC SQL
021900071212     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
022000071212     C/END-EXEC
022100000000     C*
022200000920     C                   exsr      procedi
022300050404     C*
022400050404     C* Effettuo lancio TISI95 solo x chiusura
022500050404     C                   clear                   TISI95DS
022600050404     C                   eval      I95TLA = 'C'
022700050404     C                   call      'TISI95R'
022800050404     C                   parm                    TISI95DS
022900000914     C*
023000000914     C                   SETON                                        LR
023100000922     C*------------------------------------------------------------------------*
023200000922     C* PROCEDI - Routine principale
023300000922     C*------------------------------------------------------------------------*
023400000922     C     PROCEDI       BEGSR
023500000922     C*
023600030307     C* CLRPFM del file (WFPTL00F)
023700030306     C                   Z-ADD     80            LENGH            15 5
023800071231     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023900030307     C                   PARM                    CMD(1)
024000030307     C                   PARM                    LENGH
024100071231     C*
024200071231     C* Apro il file (WFPTL00F)
024300071231     C                   open      wfptl00f
024400000922     C*
024500000922     C* Se riuscito comando ==> procedo
024600000922     C                   if        not %error
024700001211     C                   if        PARCOF = 'C'
024800030307     C                   exsr      exesald
024900001211     C                   endif
025000050404     C                   if        PARCOF = 'F'
025100050404     C                   exsr      exesaldforn
025200050404     C                   endif
025300001211     C                   eval      SQLvar = 'clnksc<>clnclifatt and '       +
025400000926     C                             'clnclifatt <> ''                '' and '
025500030307     C                   exsr      exeana
025600001211     C                   eval      SQLvar = '(clnksc=clnclifatt or '        +
025700001211     C                             'clnclifatt=''                '') and '
025800030307     C                   exsr      exeana
025900011016     C                   exsr      exechk
026000030307     C                   exsr      exeanaorg
026100030307     C                   exsr      exeinduniq
026200050406     C                   if        PARCOF = 'C'
026300050405     C                   exsr      exedel888999
026400050405     C                   endif
026500000922     C                   endif
026600071231     C*
026700071231     C* Chiudo il file (WFPTL00F)
026800071231     C                   close     wfptl00f
026900000922     C*
027000000922     C                   ENDSR
027100000914     C*------------------------------------------------------------------------*
027200000922     C* EXESALD - Routine di elaborazione saldi
027300000914     C*------------------------------------------------------------------------*
027400000922     C     EXESALD       BEGSR
027500000914     C*
027600000922     C* Apro il file (WFPTL01L)
027700000922     C                   open      wfptl01l
027800000914     C*
027900000914     C* Scompongo i parametri ricevuti in input
028000060717     C                   movel     PARDAI        didat6
028100060717     C                   movel     PARDAF        dfdat6
028200000914     C*
028300000914     C/EXEC SQL
028400000928     C+ declare C1 cursor for
028500000928     C+ select sdoksc, sum(sdoirb) from sisdo00f where
028600060717     C+ concat(digits(sdoann),digits(sdomes)) >= :didat6 and
028700060717     C+ concat(digits(sdoann),digits(sdomes)) <= :dfdat6
028800000928     C+ group by sdoksc
028900000928     C+ for read only
029000000914     C/END-EXEC
029100000914     C
029200000914     C/EXEC SQL
029300000914     C+ open C1
029400000914     C/END-EXEC
029500000914     C
029600000914     C/EXEC SQL
029700000922     C+ Fetch C1 into :dssaldi
029800000914     C/END-EXEC
029900000914     C*
030000000914     C                   dow       sqlcod = *zeros
030100000922     C                   clear                   wfptl000
030200050405     C                   movel     PARCOF        wfptip
030300050405     C                   move(p)   '201'         wfpsoc
030400000922     C                   z-add     §sdoksc       wfpksc
030500000922     C                   z-add     §sdofat       wfpfat
030600000922     C                   write     wfptl000
030700000914     C*
030800000914     C/EXEC SQL
030900000922     C+ Fetch C1 into :dssaldi
031000000914     C/END-EXEC
031100000914     C*
031200000914     C                   enddo
031300000914     C*
031400000914     C/EXEC SQL
031500000914     C+ close C1
031600000914     C/END-EXEC
031700000914     C
031800000914     C*
031900000922     C* Chiudo il file (WFPTL01L)
032000000922     C                   close     wfptl01l
032100000920     C*
032200000914     C                   ENDSR
032300050404     C*------------------------------------------------------------------------*
032400050404     C* EXESALDFORN - Routine di elaborazione saldi fornitori (da file IVA contabilità)
032500050404     C*------------------------------------------------------------------------*
032600050404     C     EXESALDFORN   BEGSR
032700050404     C*
032800050404     C* Apro il file (WFPTL01L)
032900050404     C                   open      wfptl01l
033000050404     C*
033100050404     C* Scompongo i parametri ricevuti in input
033200050404     C     *ISO          move      PARDAI        didatforn
033300050404     C     *ISO          move      PARDAF        dfdatforn
033400050404     C*
033500050404     C/EXEC SQL
033600050404     C+ declare C3 cursor for
033700050404     C+ SELECT F.IVASOCIETA, F.IVAKCCCP, F.IVAKSCCP,
033800050404     C+ DEC(SUM(F.IVAIMPONIB*(IVADARE-IVAAVERE)), 11, 2) FATTURATO,
033900050404     C+ MAX(F.IVADTDOC) ULTIMA_FT ,
034000050404     C+ MAX(IFNULL(N.NOTE_ACCREDITO, 0)) NOTE_ACCREDITO,
034100050404     C+ MAX(IFNULL(N.ULTIMA, '0001-01-01')) ULTIMA_NA
034200050404     C+ FROM NDIVA00F F LEFT JOIN (SELECT
034300050404     C+ IVASOCIETA, IVAKCCCP, IVAKSCCP,
034400050404     C+ DEC(SUM(IVAIMPONIB*(IVADARE-IVAAVERE)), 11, 2) NOTE_ACCREDITO,
034500050404     C+ MAX(IVADTDOC) ULTIMA FROM NDIVA00F JOIN ANOPE00F ON IVASOCIETA =
034600050404     C+ OPESOCIETA AND IVACAUSTES = OPECAUSALE WHERE IVATPREG = '1' AND
034700050404     C+ IVADTDOC >= :didatforn AND IVADTDOC <= :dfdatforn
034800050404     C+ AND IVADARE = 0 AND OPETPDOCI IN
034900050404     C+ ('1','4','7') GROUP BY IVASOCIETA, IVAKCCCP, IVAKSCCP)
035000050404     C+ N ON F.IVASOCIETA = N.IVASOCIETA
035100050404     C+ AND F.IVAKCCCP = N.IVAKCCCP AND F.IVAKSCCP = N.IVAKSCCP JOIN
035200050404     C+ ANOPE00F ON F.IVASOCIETA=OPESOCIETA AND F.IVACAUSTES=OPECAUSALE
035300050404     C+ WHERE F.IVATPREG = '1' AND
035400050404     C+ F.IVADTDOC >= :didatforn AND F.IVADTDOC <= :dfdatforn AND
035500050404     C+ F.IVADARE = 1 AND OPETPDOCI IN ('0','3','5')
035600050404     C+ GROUP BY F.IVASOCIETA,
035700050404     C+ F.IVAKCCCP, F.IVAKSCCP
035800050404     C+ ORDER BY
035900050404     C+ F.IVASOCIETA, F.IVAKCCCP, F.IVAKSCCP
036000050404     C+ for read only
036100050404     C/END-EXEC
036200050404     C
036300050404     C/EXEC SQL
036400050404     C+ open C3
036500050404     C/END-EXEC
036600050404     C
036700050404     C/EXEC SQL
036800050404     C+ Fetch C3 into :dssaldiforn
036900050404     C/END-EXEC
037000050404     C*
037100050404     C                   dow       sqlcod = *zeros
037200050404     C                   clear                   wfptl000
037300050405     C                   movel     PARCOF        wfptip
037400050405     C                   move(p)   §ivasocieta   wfpsoc
037500050404     C                   move(p)   §ivaksccp     wfpksc
037600050404     C                   eval      wfpfat = §ivafatturato +
037700050404     C                                      §ivanotaaccr
037800050404     C                   write     wfptl000
037900050404     C*
038000050404     C/EXEC SQL
038100050404     C+ Fetch C3 into :dssaldiforn
038200050404     C/END-EXEC
038300050404     C*
038400050404     C                   enddo
038500050404     C*
038600050404     C/EXEC SQL
038700050404     C+ close C3
038800050404     C/END-EXEC
038900050404     C
039000050404     C*
039100050404     C* Chiudo il file (WFPTL01L)
039200050404     C                   close     wfptl01l
039300050404     C*
039400050404     C                   ENDSR
039500000922     C*------------------------------------------------------------------------*
039600000922     C* EXEANA  - Routine di elaborazione anagrafico
039700000922     C*------------------------------------------------------------------------*
039800000922     C     EXEANA        BEGSR
039900000922     C*
040000060222     C* Apro i file (WFPTL01L e WFPTL07L)
040100000922     C                   open      wfptl01l
040200060222     C                   open      wfptl07l
040300000922     C*
040400001211     C* Inizializzo le variabili stringhe per la composizione dell'SQL
040500001211     C                   clear                   SQLline
040600001211     C                   eval      %len(SQLline) = 0
040700001211     C                   clear                   SQLline2
040800001211     C                   eval      %len(SQLline2)= 0
040900001211     C                   clear                   SQLcond
041000001211     C                   eval      %len(SQLcond) = 0
041100001211     C                   clear                   SQLcond2
041200001211     C                   eval      %len(SQLcond2)= 0
041300001211     C                   clear                   SQLord
041400001211     C                   eval      %len(SQLord)  = 0
041500001211     C*
041600001211     C                   eval      SQLline = 'select clnsocieta, clnsogg, '    +
041700000922     C                             'clnksc, sogdes, sogpartiva, sogcdfisc, '   +
041800000922     C                             'indindriz, indlocalit, indcap, indprov, '  +
041900001110     C                             'indstato, rcosocaval, clnclifatt, '        +
042000060615     C                             'clnclasabc, indtelefon, clnkcc, '          +
042100060615     C                             'sogdtinvl '                                +
042200000925     C                             'from (((ancln00f join ansog00f on '        +
042300001211     C                             'clnsogg=sogsogg) join anind00f on clnsogg' +
042400000925     C                             '=indsogg) join anrco00f on clnsocieta='    +
042500000925     C                             'rcosocieta and clnkcc=rcokcc and '         +
042600000926     C                             'clnksc=rcoksc) where '                     +
042700001211     C                             SQLvar                                      +
042800001211     C                             'rcosocaval=''   '' and indtpind=''  '' '   +
042900001211     C                             'and indcdind=''   '' '
043000000922     C*
043100001207     C                   if        PARCOF <> *blanks
043200001207     C                   eval      SQLcond = %trim(SQLcond)+
043300001207     C                                       ' and rcosnatura='+''''+PARCOF+''''
043400001207     C                   endif
043500001207     C                   if        PARKSC = *blanks
043600000927     C                   if        PARCAP<>*blanks
043700000927     C                   eval      SQLcond = %trim(SQLcond)+
043800001207     C                                       ' and indcap='+''''+PARCAP+''''
043900000927     C                   endif
044000000927     C                   if        PARPRV<>*blanks
044100000927     C                   eval      SQLcond = %trim(SQLcond)+
044200001207     C                                       ' and indprov='+''''+PARPRV+''''
044300000927     C                   endif
044400000927     C                   if        PARNAZ<>'999'
044500000927     C                   if        PARNAZ='   '
044600000927     C                   eval      SQLcond = %trim(SQLcond)+
044700001211     C                             ' and indstato IN (''IT  '', ''    '')'
044800000927     C                   else
044900000927     C                   eval      SQLcond = %trim(SQLcond)+
045000001207     C                                       ' and indstato='+''''+PARNAZ+''''
045100000927     C                   endif
045200000927     C                   endif
045300001207     C                   if        PARSOC <> *blanks
045400001207     C                   eval      SQLcond = %trim(SQLcond)+
045500001211     C                                       ' and rcosocieta='+''''+PARSOC+''''
045600001207     C                   else
045700000922     C                   if        PARBAR='S' and PARSDI='N' and PARPIC='N'
045800000927     C                   eval      SQLcond = %trim(SQLcond)+
045900001211     C                                       ' and (rcosocieta = ''201'')'
046000000922     C                   endif
046100000922     C                   if        PARBAR='S' and PARSDI='S' and PARPIC='N'
046200000927     C                   eval      SQLcond = %trim(SQLcond)+
046300001211     C                                       ' and (rcosocieta = ''201'' or ' +
046400001211     C                                       'rcosocieta = ''101'')'
046500000922     C                   endif
046600000922     C                   if        PARBAR='N' and PARSDI='S' and PARPIC='S'
046700000927     C                   eval      SQLcond = %trim(SQLcond)+
046800001211     C                                       ' and (rcosocieta <> ''201'')'
046900000922     C                   endif
047000000922     C                   if        PARBAR='N' and PARSDI='S' and PARPIC='N'
047100000927     C                   eval      SQLcond = %trim(SQLcond)+
047200001211     C                                       ' and (rcosocieta = ''101'')'
047300000922     C                   endif
047400000925     C                   if        PARBAR='N' and PARSDI='N' and PARPIC='S'
047500000927     C                   eval      SQLcond = %trim(SQLcond)+
047600001211     C                                       ' and (rcosocieta <> ''201'' '+
047700001211     C                                       'and  rcosocieta <> ''101'')'
047800000925     C                   endif
047900001207     C                   endif
048000001211     C                   eval      SQLcond2= %trim(SQLcond)
048100000927     C                   else
048200001211     C                   eval      SQLcond2= %trim(SQLcond)
048300000927     C                   eval      SQLcond = %trim(SQLcond)+
048400001207     C                                     ' and clnksc='+''''+'0'+PARKSC+''''
048500000927     C                   endif
048600000922     C*
048700001211     C                   eval      SQLline2 = 'union select frnsocieta, '      +
048800001211     C                             'frnsogg, frnksc, '                         +
048900001211     C                             'sogdes, sogpartiva, sogcdfisc, '           +
049000001211     C                             'indindriz, indlocalit, indcap, indprov, '  +
049100001211     C                             'indstato, rcosocaval, frnforfatt, '        +
049200060615     C                             ''' '', indtelefon, frnkcc, '               +
049300060615     C                             'sogdtinvl '                                +
049400001211     C                             'from (((anfrn00f join ansog00f on '        +
049500001211     C                             'frnsogg=sogsogg) join anind00f on frnsogg' +
049600001211     C                             '=indsogg) join anrco00f on frnsocieta='    +
049700001211     C                             'rcosocieta and frnkcc=rcokcc and '         +
049800001211     C                             'frnksc=rcoksc) where '                     +
049900001211     C                             'rcosocaval=''   '' and indtpind=''  '' '   +
050000001211     C                             'and indcdind=''   '' '
050100001211     C*
050200001211     C                   eval      SQLord = ' order by 5, 6'
050300000922     C*
050400001211     C                   eval      SQLline= %trim(SQLline) +' '+%trim(SQLcond) +
050500001211     C                                      ' '                                +
050600001211     C                                      %trim(SQLline2)+' '+%trim(SQLcond2)+
050700001211     C                                      ' '+%trim(SQLord)
050800000922     C*
050900000922     C/EXEC SQL
051000000922     C+ prepare S2 from :SQLline
051100000922     C/END-EXEC
051200000922     C*
051300000922     C*
051400000922     C/EXEC SQL
051500000922     C+ declare C2 cursor for S2
051600000922     C/END-EXEC
051700000922     C*
051800000922     C*
051900000922     C/EXEC SQL
052000000922     C+ open C2
052100000922     C/END-EXEC
052200000922     C*
052300000922     C*
052400000922     C/EXEC SQL
052500000922     C+ fetch C2 into :dspjana
052600000922     C/END-EXEC
052700000922     C*
052800000922     C                   dow       sqlcod = *zeros
052900000928     C                   exsr      chkrec
053000000928     C                   if        errl00 = '0'
053100000922     C                   exsr      valana
053200000928     C                   endif
053300000922     C*
053400000922     C/EXEC SQL
053500000922     C+ Fetch C2 into :dspjana
053600000922     C/END-EXEC
053700000922     C*
053800000922     C                   enddo
053900000922     C*
054000000922     C/EXEC SQL
054100000922     C+ close C2
054200000922     C/END-EXEC
054300000922     C
054400000922     C*
054500060222     C* Chiudo i file (WFPTL01L e WFPTL07L)
054600000922     C                   close     wfptl01l
054700060222     C                   close     wfptl07l
054800000922     C*
054900000922     C                   ENDSR
055000071228     C*------------------------------------------------------------------------*
055100071228     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
055200071228     C*------------------------------------------------------------------------*
055300071228     C     RTVPARENT     BEGSR
055400071228     C*
055500071228     C* Reperisco il padre del codice cliente corrente
055600071228     C                   CLEAR                   TIBS10DS
055700071228     C                   EVAL      D10TLE = 'ST'
055800071228     C                   EVAL      D10PAF = 'P'
055900071228     C                   MOVE(P)   §PJKSC        D10COD
056000071228     C                   CALL      'TIBS10R'
056100071228     C                   PARM                    TIBS10DS
056200071228     C                   PARM      'E'           TIPEXE            1
056300071228     C                   IF        D10ERR = '1'
056400071228     C                   Z-ADD     D10COD        §D10COP          11 0
056500071228     C                   ELSE
056600071228     C                   Z-ADD     D10COP        §D10COP
056700071228     C                   ENDIF
056800071228     C*
056900071228     C                   ENDSR
057000000922     C*------------------------------------------------------------------------*
057100000922     C* VALANA - Routine di considerazoini e valorizzazione campi anagrafici
057200000922     C*------------------------------------------------------------------------*
057300000922     C     VALANA        BEGSR
057400000922     C*
057500000926     C                   if        §PJAVA = *blanks
057600000926     C*
057700000925     C                   if        §PJCLF = *blanks
057800000925     C                   eval      §PJCLF = §PJKSC
057900000925     C                   endif
058000000925     C*
058100000926     C                   z-add     *zeros        contarec          1 0
058200000926     C*
058300050404     C                   move      §PJSOC        KWPSOC
058400000926     C                   move      §PJKSC        KWPKSC
058500050404     C     KEYPTL01      setll     wfptl01l
058600050404     C     KEYPTL01      reade     wfptl01l
058700000926     C                   dow       not %eof(wfptl01l)
058800000926     C                   z-add     1             contarec
058900060222     C                   if        wfpchk <> 'X'
059000071228     C                   eval      w§PJCLF = §PJKSC
059100071228     C                   if        (PAROVR = 'F' AND §PJKSC <> §PJCLF) OR
059200071228     C                             PAROVR = 'U'
059300050404     C                   exsr      ovrvalrec
059400011127     C                   endif
059500060222     C                   movel     PARCOF        wfptip
059600060222     C*
059700071228     C* Alla fine imposto il codice cliente da considerare (sia esso KSC/FAT/UNI)
059800071228     C                   move (p)  w§PJCLF       wfpksc
059900060222     C*
060000050405     C* Se lancio x clienti reperisco i dati del commerciale relativo
060100050405     C                   if        PARCOF = 'C'
060200011127     C                   exsr      exeage
060300050405     C                   endif
060400050405     C*
060500011127     C                   exsr      valrec
060600000926     C                   update    wfptl000
060700060222     C                   endif
060800050404     C     KEYPTL01      reade     wfptl01l
060900000926     C                   enddo
061000000926     C*
061100000926     C                   if        contarec = *zeros
061200000926     C                   clear                   wfptl000
061300071228     C                   eval      w§PJCLF = §PJKSC
061400071228     C                   if        (PAROVR = 'F' AND §PJKSC <> §PJCLF) OR
061500071228     C                             PAROVR = 'U'
061600050404     C                   exsr      ovrvalrec
061700011127     C                   endif
061800050405     C                   movel     PARCOF        wfptip
061900071228     C*
062000071228     C* Alla fine imposto il codice cliente da considerare (sia esso KSC/FAT/UNI)
062100071228     C                   move (p)  w§PJCLF       wfpksc
062200060222     C*
062300050405     C* Se lancio x clienti reperisco i dati del commerciale relativo
062400050405     C                   if        PARCOF = 'C'
062500011127     C                   exsr      exeage
062600050405     C                   endif
062700060222     C*
062800060222     C* Salvo sul record il tipo blocco cliente
062900071228     C                   eval      wfptbc = flgblk
063000060222     C*
063100060222     C* Eseguo aggiornamento x indicare il codice blocco su tutti i record aventi medesimo
063200071228     C* codice cliente (ogni codice cliente deve mantenere cmq il suo codice blocco,
063300071228     C* successivamente se tutti i codici clienti della stessa PIVA sono bloccati verranno
063400071231     C* tutti eliminati - vedi vista logica WFPTL02L).
063500071228     C*    wfpksc        setll     wfptl07l
063600071228     C*    wfpksc        reade     wfptl07l
063700071228     C*                  dow       not %eof(wfptl07l)
063800071228     C*                  eval      K_wfptbc = flgblk
063900071228     C*                  update    wfptl007
064000071228     C*    wfpksc        reade     wfptl07l
064100071228     C*                  enddo
064200050405     C*
064300000926     C                   exsr      valrec
064400000926     C                   write     wfptl000
064500000926     C                   endif
064600000926     C                   endif
064700000922     C*
064800000922     C                   ENDSR
064900000922     C*------------------------------------------------------------------------*
065000000922     C* VALREC - Routine di valorizzazione buffer del record
065100000922     C*------------------------------------------------------------------------*
065200000922     C     VALREC        BEGSR
065300050406     C*
065400050406     C* Solo se lancio x fornitori reperisco la classificazione attività
065500050406     C                   if        PARCOF = 'F'
065600050406     C     KEYRCO01L     chain     ANRCO01L
065700050406     C                   if        %found(ANRCO01L)
065800050406     C                   eval      §PJABC = %trim(F_RCOCTGAN02)
065900050406     C                   endif
066000050406     C                   endif
066100000922     C*
066200000922     C* Valorizzo solo i campi del buffer del record che mi interessano
066300000922     C                   movel(p)  §PJDES        wfprag
066400000922     C                   movel(p)  §PJIND        wfpind
066500000922     C                   movel(p)  §PJLOC        wfploc
066600000922     C                   movel(p)  §PJCAP        wfpcap
066700000922     C                   movel(p)  §PJPRV        wfppro
066800001110     C                   movel(p)  §PJABC        wfpabc
066900001114     C                   movel(p)  §PJTEL        wfptel
067000050404     C                   movel(p)  §PJSOC        wfpsoc
067100000922     C                   if        §PJNAZ='IT '
067200000922     C                   movel(p)  *blanks       wfpnaz
067300000922     C                   else
067400000922     C                   movel(p)  §PJNAZ        wfpnaz
067500000922     C                   endif
067600061114     C*
067700061114     C* X le successive considerazioni battezzo o la P.IVA o il COD.FISC.
067800071231     C                   if        §PJIVA <> *blanks AND
067900071231     C                             %trim(§PJIVA) <> '$$'
068000000922     C                   movel(p)  §PJIVA        wfpioc
068100000922     C                   else
068200000922     C                   movel(p)  §PJCDF        wfpioc
068300000922     C                   endif
068400061114     C*
068500061114     C* Ma memorizzo cmq entrambi i dati
068600061114     C                   movel(p)  §PJIVA        wfppiva
068700061114     C                   movel(p)  §PJCDF        wfpcodf
068800060615     C*
068900060616     C* Se richiesto nei parametri d lancio....
069000060616     C* ...determino se il soggetto è "nuovo" (ovvero se la data attivazione in anagrafica è +
069100060616     C* recente rispetto alla data "A" del periodo fatturazione indicato nei parametri d lancio,
069200060616     C* ri-ovvero dell'ultimo periodo saldi cliente)
069300060616     C                   if        PARSOGN = 'S'
069400060615     C                   movel     *blanks       wDataCliAttAlf    8
069500060615     C                   z-add     *zeros        wDataCliAttNum    8 0
069600060615     C                   eval      wDataCliAttAlf = %subst(§PJDTINVL: 1: 4) +
069700060615     C                                              %subst(§PJDTINVL: 6: 2) +
069800060615     C                                              %subst(§PJDTINVL: 9: 2)
069900060615     C                   move(p)   wDataCliAttAlfwDataCliAttNum
070000060616     C*
070100060616     C* Sottraggo 1 ulteriore mese dalla data "A"
070200060616     C     *ISO          MOVE      PARDAF        findata
070300060619     C     findata       SUBDUR    2:*M          findata
070400060616     C     *ISO          MOVE      findata       wDataNewSogg      8 0
070500060616     C*
070600060616     C* Verifico quindi se + recente
070700060616     C                   if        wDataCliAttNum >= wDataNewSogg
070800061114     C                   eval      wfpson = 'S'
070900060615     C                   endif
071000060616     C                   endif
071100010914     C*
071200050711     C* Solo se lancio x clienti effettuo le chain
071300050405     C                   if        PARCOF = 'C'
071400060619     C                   movel     *blanks       wSavTNT           2
071500060619     C                   movel     *blanks       wSavTNT_01        2
071600060619     C                   movel     *blanks       wSavTNT_02        2
071700060619     C                   movel     *blanks       wSavTNT_03        2
071800060619     C                   movel     *blanks       wSavTNT_04        2
071900060619     C                   movel     *blanks       wSavTNT_05        2
072000060619     C                   movel     *blanks       wSavTNT_06        2
072100060619     C                   movel     *blanks       wSavTNT_07        2
072200060619     C                   movel     *blanks       wSavTNT_08        2
072300060619     C                   movel     *blanks       wSavRNT_01       60
072400060619     C                   movel     *blanks       wSavRNT_02       60
072500060619     C                   movel     *blanks       wSavRNT_03       60
072600060619     C                   movel     *blanks       wSavRNT_04       60
072700060619     C                   movel     *blanks       wSavRNT_05       60
072800060619     C                   movel     *blanks       wSavRNT_06       60
072900060619     C                   movel     *blanks       wSavRNT_07       60
073000060619     C                   movel     *blanks       wSavRNT_08       62
073100050711     C*
073200050711     C* ...x reperire i campi relativi all' e-mail
073300020923     C                   eval      KNTNK1 = ALFAKCI + %subst(§PJKSC:2:7)
073400050711     C     KEYNTC_P      setll     tfntc01l
073500050711     C                   if        %equal(tfntc01l)
073600050711     C     KEYNTC_P      reade     tfntc01l
073700050711     C                   dow       not %eof(tfntc01l)
073800050711     C     ntcflt        ifne      'A'
073900050711     C                   if        ntctnt  = PARNTE1
074000060619     C                   eval      wSavTNT_01 = ntctnt
074100060619     C                   eval      wSavRNT_01 = ntcrnt
074200060619     C***                eval      wfpeml  = ntcrnt
074300060619     C***                leave
074400050711     C                   else
074500050711     C                   if        ntctnt  = PARNTE2
074600060619     C                   eval      wSavTNT_02 = ntctnt
074700060619     C                   eval      wSavRNT_02 = ntcrnt
074800060619     C***                eval      wfpeml  = ntcrnt
074900060619     C***                leave
075000050711     C                   else
075100050711     C                   if        ntctnt  = PARNTE3
075200060619     C                   eval      wSavTNT_03 = ntctnt
075300060619     C                   eval      wSavRNT_03 = ntcrnt
075400060619     C***                eval      wfpeml  = ntcrnt
075500060619     C***                leave
075600050711     C                   else
075700050711     C                   if        ntctnt  = PARNTE4
075800060619     C                   eval      wSavTNT_04 = ntctnt
075900060619     C                   eval      wSavRNT_04 = ntcrnt
076000060619     C***                eval      wfpeml  = ntcrnt
076100060619     C***                leave
076200050711     C                   else
076300050711     C                   if        ntctnt  = PARNTE5
076400060619     C                   eval      wSavTNT_05 = ntctnt
076500060619     C                   eval      wSavRNT_05 = ntcrnt
076600060619     C***                eval      wfpeml  = ntcrnt
076700060619     C***                leave
076800050711     C                   else
076900050711     C                   if        ntctnt  = PARNTE6
077000060619     C                   eval      wSavTNT_06 = ntctnt
077100060619     C                   eval      wSavRNT_06 = ntcrnt
077200060619     C***                eval      wfpeml  = ntcrnt
077300060619     C***                leave
077400050711     C                   else
077500050711     C                   if        ntctnt  = PARNTE7
077600060619     C                   eval      wSavTNT_07 = ntctnt
077700060619     C                   eval      wSavRNT_07 = ntcrnt
077800060619     C***                eval      wfpeml  = ntcrnt
077900060619     C***                leave
078000050711     C                   else
078100050711     C                   if        ntctnt  = PARNTE8
078200060619     C                   eval      wSavTNT_08 = ntctnt
078300060619     C                   eval      wSavRNT_08 = ntcrnt
078400060619     C***                eval      wfpeml  = ntcrnt
078500060619     C***                leave
078600050711     C                   endif
078700050711     C                   endif
078800050711     C                   endif
078900050711     C                   endif
079000050711     C                   endif
079100050711     C                   endif
079200050711     C                   endif
079300050711     C                   endif
079400050711     C*
079500050711     C                   endif
079600050711     C     KEYNTC_P      reade     tfntc01l
079700050711     C                   enddo
079800050711     C                   endif
079900060619     C*
080000060619     C* Verifico priorità note reperite
080100060619     C                   if        wSavTNT_01 <> *blanks
080200060619     C                   eval      wSavTNT = wSavTNT_01
080300060619     C                   eval      wfpeml  = wSavRNT_01
080400060619     C                   else
080500060619     C                   if        wSavTNT_02 <> *blanks
080600060619     C                   eval      wSavTNT = wSavTNT_02
080700060619     C                   eval      wfpeml  = wSavRNT_02
080800060619     C                   else
080900060619     C                   if        wSavTNT_03 <> *blanks
081000060619     C                   eval      wSavTNT = wSavTNT_03
081100060619     C                   eval      wfpeml  = wSavRNT_03
081200060619     C                   else
081300060619     C                   if        wSavTNT_04 <> *blanks
081400060619     C                   eval      wSavTNT = wSavTNT_04
081500060619     C                   eval      wfpeml  = wSavRNT_04
081600060619     C                   else
081700060619     C                   if        wSavTNT_05 <> *blanks
081800060619     C                   eval      wSavTNT = wSavTNT_05
081900060619     C                   eval      wfpeml  = wSavRNT_05
082000060619     C                   else
082100060619     C                   if        wSavTNT_06 <> *blanks
082200060619     C                   eval      wSavTNT = wSavTNT_06
082300060619     C                   eval      wfpeml  = wSavRNT_06
082400060619     C                   else
082500060619     C                   if        wSavTNT_07 <> *blanks
082600060619     C                   eval      wSavTNT = wSavTNT_07
082700060619     C                   eval      wfpeml  = wSavRNT_07
082800060619     C                   else
082900060619     C                   if        wSavTNT_08 <> *blanks
083000060619     C                   eval      wSavTNT = wSavTNT_08
083100060619     C                   eval      wfpeml  = wSavRNT_08
083200060619     C                   endif
083300060619     C                   endif
083400060619     C                   endif
083500060619     C                   endif
083600060619     C                   endif
083700060619     C                   endif
083800060619     C                   endif
083900060619     C                   endif
084000050711     C*
084100050711     C* Inserisco veirifica x modifica carattere § in @
084200050711     C     '§':'@'       xlate     wfpeml        wfpeml
084300060207     C                   eval      wfptne = wSavTNT
084400050711     C*
084500050711     C* ...x reperire i campi relativi al responsabile SOLO X TIPO NOTA 02 o 05
084600050711     C* ...ovvero se tipo nota e-mail reperito in precedenza è 03 o 06
084700050711     C                   if        wSavTNT = '03'
084800050711     C                   eval      KNTTNT = '02'
084900050711     C     KEYNTC        chain     tfntc01l
085000050711     C                   if        %found(tfntc01l)
085100050711     C     ntcflt        ifne      'A'
085200050711     C                   eval      wfprsp = ntcrnt
085300060207     C                   eval      wfptnr = ntctnt
085400050711     C                   endif
085500050711     C                   endif
085600050711     C                   endif
085700050711     C*
085800050711     C                   if        wSavTNT = '06'
085900050711     C                   eval      KNTTNT = '05'
086000050711     C     KEYNTC        chain     tfntc01l
086100050711     C                   if        %found(tfntc01l)
086200050711     C     ntcflt        ifne      'A'
086300050711     C                   eval      wfprsp = ntcrnt
086400061114     C                   eval      wfptnt = ntctnt
086500050711     C                   endif
086600050711     C                   endif
086700050711     C                   endif
086800050711     C*
086900050711     C*    KEYNTC        setll     tfntc01l
087000050711     C*                  if        %equal(tfntc01l)
087100050711     C*    KEYNTC        reade     tfntc01l
087200050711     C*                  dow       not %eof(tfntc01l)
087300050711     C*    ntcflt        ifne      'A'
087400050711     C*                  if        ntctnt = PARNTN1
087500050711     C*                  eval      wfprsp = ntcrnt
087600050711     C*                  leave
087700050711     C*                  else
087800050711     C*                  if        ntctnt = PARNTN2
087900050711     C*                  eval      wfprsp = ntcrnt
088000050711     C*                  leave
088100050711     C*                  else
088200050711     C*                  if        ntctnt = PARNTN3
088300050711     C*                  eval      wfprsp = ntcrnt
088400050711     C*                  leave
088500050711     C*                  else
088600050711     C*                  if        ntctnt = PARNTN4
088700050711     C*                  eval      wfprsp = ntcrnt
088800050711     C*                  leave
088900050711     C*                  endif
089000050711     C*                  endif
089100050711     C*                  endif
089200050711     C*                  endif
089300050711     C*
089400050711     C*                  endif
089500050711     C*    KEYNTC        reade     tfntc01l
089600050711     C*                  enddo
089700050711     C                   endif
089800000922     C*
089900000922     C                   movel(p)  'X'           wfpchk
090000000922     C*
090100000922     C                   ENDSR
090200011127     C*------------------------------------------------------------------------*
090300011127     C* OVRVALREC - Routine di sovrapposizione valori di valorizzazione buffer del record
090400011127     C*------------------------------------------------------------------------*
090500011127     C     OVRVALREC     BEGSR
090600011127     C*
090700050405     C* Reperisco i dati del soggetto corrente
090800020923     C                   IF        §PJKCC = '00'+ALFAKCI
090900020311     C                   EVAL      wrknatura = 'C'
091000020311     C                   ENDIF
091100050406     C                   IF        §PJKCC = '000400'
091200050406     C                   EVAL      wrknatura = 'F'
091300050406     C                   ENDIF
091400060222     C*
091500060222     C* Se se richiesto espressamente nel lancio sovrapposizione cliente fatturazine a cliente KSC
091600060222     C* ...altrimenti valorizzo coi dati del cliente KSC
091700071228     C                   if        PAROVR = 'F'
091800060222     C                   eval      w§PJCLF = §PJCLF
091900060222     C                   else
092000071228     C                   if        PAROVR = 'U'
092100071228     C                   exsr      RTVPARENT
092200071228     C                   move(p)   §D10COP       w§PJCLF
092300060222     C                   endif
092400071228     C                   endif
092500020311     C*
092600050406     C     KEYRCO98J     CHAIN     ANRCO98J
092700050404     C     KEYCLN        CHAIN     ANCLN01L
092800011127     C*
092900011127     C* Dopo aver reperito i dati eseguo appunto le sovrapposizioni di valori
093000020311     C                   EVAL      §PJDES = SOGDES
093100020311     C                   EVAL      §PJIND = INDINDRIZ
093200020311     C                   EVAL      §PJLOC = INDLOCALIT
093300020311     C                   EVAL      §PJCAP = INDCAP
093400020311     C                   EVAL      §PJPRV = INDPROV
093500050404     C                   IF        wrknatura = 'C'
093600020311     C                   EVAL      §PJABC = CLNCLASABC
093700050404     C                   ENDIF
093800020311     C                   EVAL      §PJTEL = INDTELEFON
093900020311     C                   EVAL      §PJNAZ = INDSTATO
094000020311     C                   EVAL      §PJIVA = SOGPARTIVA
094100020311     C                   EVAL      §PJCDF = SOGCDFISC
094200011127     C*
094300011127     C                   ENDSR
094400000922     C*------------------------------------------------------------------------*
094500000922     C* EXECHK  - Routine di considerazioni finali
094600000922     C*------------------------------------------------------------------------*
094700000922     C     EXECHK        BEGSR
094800060209     C*
094900060209     C*****
095000060209     C* 1° GIRO DI CONTROLLO (nn considero, ergo elimino, i record aventi codice commerciale
095100060209     C*                       "vari" o "inattivi" ovvero quelli con XXX0000 o XXX0999)
095200060209     C*****
095300060209     C*
095400060209     C     *start        setll     wfptl00f
095500060209     C                   dow       not %eof(wfptl00f)
095600060209     C                   read      wfptl00f
095700060209     C                   if        not %eof(wfptl00f)
095800071231     C                   move(p)   f_wfpcom      wCom              4
095900060209     C                   if        wCom = '0000' OR wCom = '0999'
096000060209     C                   delete    wfptl00f
096100060209     C                   endif
096200060209     C                   endif
096300060209     C                   enddo
096400030306     C*
096500030306     C*****
096600060209     C* 2° GIRO DI CONTROLLO (attribuzione e-mail e responsabile dei soggetti non "battezzati"
096700050405     C*                       come unificanti ai soggetti invece unificanti che
096800050405     C*                       eventualmente ne fossero sprovvisti (naturalmente unificanti
096900060221     C*                       x medesima P.IVA/C.FISC). => solo se richiesto nei parametri
097000030306     C*****
097100030306     C                   eval      depioc = *loval
097200060221     C*
097300060221     C                   if        PARRGR = 'S'
097400030306     C*
097500030306     C* Apro il file (WFPTL05L)
097600030306     C                   open      wfptl05l
097700030306     C*
097800030306     C     *loval        setll     wfptl05l
097900030306     C                   dow       not %eof(wfptl05l)
098000030306     C                   read      wfptl05l
098100030306     C                   if        not %eof(wfptl05l)
098200030306     C                   if        wfpioc <> depioc
098300030306     C                   eval      depioc = wfpioc
098400030306     C                   eval      depeml = *blanks
098500030306     C                   eval      deprsp = *blanks
098600030306     C                   if        wfpeml <> *blanks
098700030306     C                   eval      depeml = wfpeml
098800030306     C                   endif
098900030306     C                   if        wfprsp <> *blanks
099000030306     C                   eval      deprsp = wfprsp
099100030306     C                   endif
099200030306     C                   else
099300030306     C                   if        wfpeml <> *blanks
099400030306     C                   eval      depeml = wfpeml
099500030306     C                   endif
099600030306     C                   if        wfprsp <> *blanks
099700030306     C                   eval      deprsp = wfprsp
099800030306     C                   endif
099900030306     C                   if        (wfpeml = *blanks  OR
100000030306     C                              wfprsp = *blanks) AND
100100030306     C                             (depeml <> *blanks OR
100200030306     C                              deprsp <> *blanks)
100300030306     C                   if        wfpeml = *blanks
100400030306     C                   eval      wfpeml = depeml
100500030306     C                   endif
100600030306     C                   if        wfprsp = *blanks
100700030306     C                   eval      wfprsp = deprsp
100800030306     C                   endif
100900030306     C                   update    wfptl005
101000030306     C                   endif
101100030306     C                   endif
101200030306     C                   endif
101300030306     C                   enddo
101400030306     C*
101500030306     C* Chiudo il file (WFPTL05L)
101600030306     C                   close     wfptl05l
101700060221     C                   endif
101800011127     C*
101900011127     C*****
102000071228     C* 3° GIRO DI CONTROLLO (raggruppamento record x medesima P.IVA/COD.FISC. con sommatoria
102100050406     C*                       dei fatturati)
102200011127     C*****
102300011127     C                   eval      depioc = *loval
102400000922     C*
102500071231     C* Apro i file (WFPTL02L)
102600000922     C                   open      wfptl02l
102700000922     C*
102800000922     C     *loval        setll     wfptl02l
102900000922     C                   dow       not %eof(wfptl02l)
103000071231     C                   read(n)   wfptl02l
103100011127     C                   if        not %eof(wfptl02l)
103200050406     C                   exsr      decodifatt
103300011127     C                   endif
103400000922     C                   enddo
103500050406     C*
103600050406     C* Dopo l'ultima lettura eseguo nuovamente il test x verificare
103700050406     C* se occorre eseguire la sommatoria del fatturato dell'ultimo record sempre
103800050406     C* x medesima P.IVA/COD.FISC.
103900071231     C                   exsr      EXESUMFATT
104000000922     C*
104100071231     C* Chiudo i file (WFPTL02L)
104200000922     C                   close     wfptl02l
104300071212     C*
104400050406     C*****
104500071228     C* 4° GIRO DI CONTROLLO (eliminazione record risultanti che dalle precedenti elaborazioni
104600050406     C*                       risultano nn conformi ai parametri di lancio)
104700050406     C*****
104800050406     C                   eval      depioc = *loval
104900050406     C*
105000050406     C* Apro il file (WFPTL02L)
105100050406     C                   open      wfptl02l
105200050406     C*
105300050406     C     *loval        setll     wfptl02l
105400050406     C                   dow       not %eof(wfptl02l)
105500050406     C                   read      wfptl02l
105600050406     C                   if        not %eof(wfptl02l)
105700050406     C                   exsr      decodi
105800050406     C                   endif
105900050406     C                   enddo
106000050406     C*
106100050406     C* Chiudo il file (WFPTL02L)
106200050406     C                   close     wfptl02l
106300011127     C*
106400011127     C*****
106500071228     C* 5° GIRO DI CONTROLLO (elimino record in modo da tenere x medesima P.IVA/C.FISC solo soggetto
106600030306     C*                       con maggior fatturato)
106700011127     C*****
106800011127     C                   eval      depksc = *loval
106900050404     C                   eval      depsoc = *loval
107000011127     C*
107100011127     C* Apro il file (WFPTL04L)
107200011127     C                   open      wfptl04l
107300011127     C*
107400011127     C     *loval        setll     wfptl04l
107500011127     C                   dow       not %eof(wfptl04l)
107600011127     C                   read      wfptl04l
107700011127     C                   if        not %eof(wfptl04l)
107800011127     C*
107900050404     C* Controllo sulla P.IVA ==> x medesima (vuota) rompo anche per società/codice soggetto
108000011127     C* e tengo buona quello avente maggior fatturato
108100050404     C                   if        wfpsoc = depsoc AND                          considerazioni
108200050404     C                             wfpksc = depksc                              considerazioni
108300011127     C                   delete    wfptl004
108400011127     C                   else
108500050404     C                   movel(p)  wfpsoc        depsoc
108600050404     C                   movel(p)  wfpksc        depksc
108700011127     C                   endif
108800011127     C                   endif
108900011127     C*
109000011127     C                   enddo
109100011127     C*
109200011127     C* Chiudo il file (WFPTL04L)
109300011127     C                   close     wfptl04l
109400060221     C*
109500000922     C                   ENDSR
109600011127     C*------------------------------------------------------------------------*
109700050405     C* EXEAGE - Routine di reperimento agente commerciale legato al cliente
109800011127     C*------------------------------------------------------------------------*
109900011127     C     EXEAGE        BEGSR
110000011127     C*
110100011127     C                   movel     §PJSOC        KBASOC
110200020923     C                   movel     *ALL'0'       KBAKCC
110300020923     C                   move      DUTKCI        KBAKCC
110400011127     C                   move      wfpksc        KBAKSC
110500011127     C     KEYYBA        chain     ybacl01l
110600011127     C                   if        %found(ybacl01l)
110700011127     C                   z-add     aclage        wfpcom
110800020220     C* Reperisco la descrizione del commerciale
110900060207     C*                  z-add     1             tblkut
111000060207     C*                  movel     '01'          tblcod
111100060207     C*                  movel(p)  wfpcom        tblkey
111200060207     C*    keytab        chain     tabel00f
111300060207     C*                  if        %found(tabel00f)
111400060207     C*                  movel     tbluni        DS01
111500060207     C*                  movel     §01AGE        wfpdec
111600060207     C*                  endif
111700060202     C* Reperisco l'e-mail del commerciale
111800060202     C                   movel     'HTM'         tbecod
111900060202     C                   movel(p)  wfpcom        tbeke1
112000060203     C                   movel(p)  'CC'          tbeke2
112100060202     C     keytbe        chain     tntbe01l
112200060202     C                   if        %found(tntbe01l)
112300060203     C                   movel     tbeuni        DHTM
112400060207     C                   movel     §HTMNMC       wfpdec
112500060207     C                   movel     §HTMIND       wfpemc
112600060202     C                   endif
112700011127     C                   endif
112800011127     C*
112900011127     C                   ENDSR
113000000928     C*------------------------------------------------------------------------*
113100000928     C* CHKREC - Routine di controllo validità del record attivo dello snapshot
113200000928     C*------------------------------------------------------------------------*
113300000928     C     CHKREC        BEGSR
113400000928     C*
113500000928     C                   movel     '0'           errl00            1
113600000928     C                   movel     *blanks       flgblk            1
113700001211     C*
113800001211     C* Verifico, qualora richiesto, che il cliente sia del P.O. indicato
113900050405     C                   if        PARFIL > *zeros AND PARCOF = 'C'
114000001211     C                   if        errl00 = '0'
114100001211     C                   movel     §PJKSC        alfksc            4
114200001211     C                   move      alfksc        numksc            3 0
114300001211     C                   if        numksc <> PARFIL
114400001211     C                   movel     '1'           errl00
114500001211     C                   endif
114600001211     C                   endif
114700001211     C                   endif
114800000928     C*
114900000928     C* Controllo che il cliente sia o non sia bloccato (a seconda dei parametri di lancio)
115000001211     C                   if        PARCOF = 'C'
115100000928     C                   if        errl00 = '0'
115200000928     C                   movel     §PJSOC        KBASOC
115300020923     C                   movel     *ALL'0'       KBAKCC
115400020923     C                   move      DUTKCI        KBAKCC
115500000928     C                   movel     §PJKSC        KBAKSC
115600000928     C     KEYYBA        chain     ybacl01l
115700000928     C                   if        %found(ybacl01l)
115800000928     C                   if        acltbc <> *blanks
115900060220     C***                movel     *hival        flgblk
116000060220     C                   movel     acltbc        flgblk
116100000928     C                   endif
116200000928     C                   endif
116300000928     C*
116400000928     C* Considerazioni finali
116500000928     C                   if        PARBLK = 'B' and flgblk =  *blanks
116600000928     C                   movel     '1'           errl00
116700000928     C                   endif
116800000928     C                   if        PARBLK = 'N' and flgblk <> *blanks
116900000928     C                   movel     '1'           errl00
117000000928     C                   endif
117100000928     C                   endif
117200001211     C                   endif
117300000928     C*
117400000928     C                   ENDSR
117500011016     C*------------------------------------------------------------------------*
117600011016     C* EXEINDUNIQ - Routine di eliminazione indirizzi doppi
117700011016     C*------------------------------------------------------------------------*
117800011016     C     EXEINDUNIQ    BEGSR
117900011016     C*
118000011016     C* Se richiesto elimino gli indirizzi doppi
118100011016     C                   if        PARIND = 'S'
118200011016     C*
118300011016     C* Apro il file (WFPTL03L)
118400011016     C                   open      wfptl03l
118500011016     C*
118600011016     C* Inizializzo i campi di deposito
118700011016     C                   eval      wrkrag = *loval
118800011016     C                   eval      wrkind = *loval
118900011016     C                   eval      wrkloc = *loval
119000011016     C                   eval      wrkpro = *loval
119100011016     C                   eval      wrkcap = *loval
119200011016     C                   eval      wrknaz = *loval
119300011016     C                   eval      countInd = *zeros
119400011016     C*
119500011016     C     *loval        setll     wfptl03l
119600011016     C                   read(e)   wfptl03l                                     1° lett. fuori ciclo
119700011016     C                   eval      countInd = countInd + 1
119800011016     C                   dow       not %eof(wfptl03l)
119900011016     C                   if        wfprag = wrkrag and
120000011016     C                             wfpind = wrkind and
120100011016     C                             wfploc = wrkloc and
120200011016     C                             wfppro = wrkpro and
120300011016     C                             wfpcap = wrkcap and
120400011016     C                             wfpnaz = wrknaz
120500011016     C                   if        countInd > *zeros
120600011016     C                   delete    wfptl03l
120700011016     C                   endif
120800011016     C                   else
120900011016     C* Riassegno i campi di deposito
121000011016     C                   eval      wrkrag = wfprag
121100011016     C                   eval      wrkind = wfpind
121200011016     C                   eval      wrkloc = wfploc
121300011016     C                   eval      wrkpro = wfppro
121400011016     C                   eval      wrkcap = wfpcap
121500011016     C                   eval      wrknaz = wfpnaz
121600011016     C                   eval      countInd = *zeros
121700011016     C                   endif
121800011016     C                   read(e)   wfptl03l                                     letture dentro ciclo
121900011016     C                   eval      countInd = countInd + 1
122000011016     C                   enddo
122100011016     C*
122200011016     C* Chiudo il file (WFPTL03L)
122300011016     C                   close     wfptl03l
122400011016     C*
122500011016     C                   endif
122600011016     C*
122700011016     C                   ENDSR
122800011127     C*------------------------------------------------------------------------*
122900011127     C* EXEDEL888999 - Routine di eliminazione codici 8888/9999
123000011127     C*------------------------------------------------------------------------*
123100011127     C     EXEDEL888999  BEGSR
123200011127     C*
123300071231     C     *start        setll     wfptl00f
123400011127     C                   read      wfptl00f                                     1° lett. fuori ciclo
123500011127     C                   dow       not %eof(wfptl00f)
123600071231     C                   move      f_wfpksc      wrkcli            4 0
123700011127     C                   if        wrkcli = 8888 or
123800011127     C                             wrkcli = 9999 or
123900071231     C                             %scan('ATTRIBUIRE':f_wfprag) > *zeros or
124000071231     C                             %scan('TARIFFA':f_wfprag) > *zeros
124100011127     C                   delete    wfptl00f
124200011127     C                   endif
124300011127     C                   read      wfptl00f                                     letture dentro ciclo
124400011127     C                   enddo
124500011127     C*
124600011127     C                   ENDSR
124700000925     C*------------------------------------------------------------------------*
124800000925     C* DECODI - Routine di effettuazione controlli di validità di contenuto
124900000925     C*------------------------------------------------------------------------*
125000000925     C     DECODI        BEGSR
125100000925     C*
125200000925     C                   movel     '0'           errl00            1
125300000925     C*
125400001128     C* Controllo che il cliente sia effettivamente di una "vera" Filiale o Agenzia solo
125500001128     C* se la società È Bartolini o SDI
125600001207     C                   if        errl00 = '0'
125700050405     C                   if        PARCOF = 'C'
125800050404     C                   if        wfpsoc = '201' or
125900050404     C                             wfpsoc = '101'
126000050405     C                   if        PARFOA = 'S'
126100001128     C                   movel(p)  wfpksc        clifil            3 0
126200001128     C     clifil        lookup    FILAGE                                 20
126300001128     C                   if        *in20  = '0' or clifil = *zeros
126400001128     C                   movel     '1'           errl00
126500001128     C                   endif
126600001128     C                   endif
126700001128     C                   endif
126800050405     C                   endif
126900001128     C                   endif
127000001211     C*
127100001211     C* Controllo che, se richiesta per 'F' (fornitori) si tratti di un fornitore effettivo
127200001211     C                   if        errl00 = '0'
127300001211     C                   if        PARCOF = 'F'
127400001211     C                   if        %subst(wfprag:1:1) = '$'
127500001211     C                   movel     '1'           errl00
127600001211     C                   endif
127700001211     C                   endif
127800001211     C                   endif
127900000925     C*
128000000925     C* Controllo che il fatturato del cliente sia conforme ai parametri di lancio
128100000925     C                   if        errl00 = '0'
128200050406     C                   if        PARFAT <> *zeros
128300060615     C                   if        wfpfat < PARFAT and
128400061114     C                             wfpson <> 'S'
128500000925     C                   movel     '1'           errl00
128600000925     C                   endif
128700050406     C                   endif
128800001211     C                   endif
128900000925     C*
129000000925     C* Controllo sulla P.IVA/COD.FISC ==> se entrambi a *blanks non considero il cliente
129100000925     C                   if        errl00 = '0'
129200001207     C                   if        PARCPI = 'N' and
129300060220     C                             (wfpioc = *blanks OR
129400060220     C                              %trim(wfpioc) = '$$')
129500000925     C                   movel     '1'           errl00
129600000925     C                   endif
129700000925     C                   endif
129800060220     C*
129900060220     C* Controllo sul flag di codice blocco cliente => se <> *blanks e richiesto nel lancio
130000060220     C* nn considero il cliente
130100060220     C                   if        errl00 = '0'
130200060222     C                   if        WFPTBC <> *blanks AND
130300060220     C                             partbc = 'S'
130400060220     C                   movel     '1'           errl00
130500060220     C                   endif
130600060220     C                   endif
130700000925     C*
130800000925     C* Controllo sul flag di check ==> se diverso da 'X' non considero
130900000925     C                   if        errl00 = '0'
131000060222     C                   if        wfpchk <> 'X' and
131100060222     C                             parchk =  'S'
131200000925     C                   movel     '1'           errl00
131300000925     C                   endif
131400000925     C                   endif
131500000925     C*
131600000925     C                   if        errl00 = '1'
131700000925     C                   delete    wfptl002
131800000925     C                   endif
131900000925     C*
132000000925     C                   ENDSR
132100060222     C*------------------------------------------------------------------------*
132200060222     C* DECODIFATT - Routine di effettuazione raggruppamento P.IVA/COD.FISC. e somma fatturato
132300060222     C*------------------------------------------------------------------------*
132400060222     C     DECODIFATT    BEGSR
132500060222     C*
132600060222     C                   if        wfpioc <> *blanks
132700060222     C                   if        wfpioc = depioc                              considerazioni
132800060222     C* Salvo l'ammontare del record corrente (che verrà eliminato) x portarlo in somma a quello
132900060222     C* del record che rimarrà (ovviamente x medesima P.IVA/COD.FISC.)
133000071231     C                   add       wfpfat        depfat
133100071231     C     wfptlnrri     chain     wfptl00f
133200071231     C                   if        %found(wfptl00f)
133300071231     C                   delete    wfptlfff
133400071231     C                   endif
133500071231     C*
133600060222     C                   else
133700071231     C* A cambio P.IVA/COD.FISC. aggiorno il fatturato sempre sul primo record
133800071231     C                   exsr      EXESUMFATT
133900060222     C                   endif
134000060222     C                   endif
134100060222     C*
134200060222     C                   ENDSR
134300071231     C*------------------------------------------------------------------------*
134400071231     C* EXESUMFATT - Routine di sommatoria e aggiornamento fatturato
134500071231     C*------------------------------------------------------------------------*
134600071231     C     EXESUMFATT    BEGSR
134700071231     C*
134800071231     C                   if        depioc <> *loval
134900071231     C     wfptlnrrp     chain     wfptl00f
135000071231     C                   if        %found(wfptl00f)
135100071231     C                   eval      f_wfpfat = depfat
135200071231     C                   update    wfptlfff
135300071231     C                   endif
135400071231     C                   endif
135500071231     C*
135600071231     C                   z-add     wfpfat        depfat
135700071231     C                   movel(p)  wfpioc        depioc
135800071231     C                   eval      wfptlnrrp = wfptlnrri
135900071231     C*
136000071231     C                   ENDSR
136100001120     C*------------------------------------------------------------------------*
136200001120     C* EXEANAORG  - Routine di attribuzione classificazione territoriale clienti (DIV/AREA/FIL)
136300001120     C*------------------------------------------------------------------------*
136400001120     C     EXEANAORG     BEGSR
136500001120     C*
136600001120     C* Apro il file (WFPTL02L)
136700001120     C                   open      wfptl02l
136800001120     C*
136900001120     C     *loval        setll     wfptl02l
137000001120     C                   read(e)   wfptl02l
137100001120     C                   dow       not %eof(wfptl02l)
137200050404     C*
137300050404     C* Se cliente
137400050404     C                   if        (PARCOF = 'C' and
137500050404     C                             (wfpsoc = '201' or wfpsoc = '101'))
137600001120     C* Estrapolo la filiale del cliente corrente
137700001120     C                   movel     wfpksc        depfil            3 0
137800001120     C     depfil        chain     azorg01l
137900020220     C                   if        %found(azorg01l)
138000001120     C                   z-add     orgfil        wfpfil
138100001120     C                   z-add     orgcar        wfpare
138200001120     C                   movel     orgfl3        wfpdiv
138300020220     C                   movel     orgdes        wfpdes
138400020220     C                   movel     orgind        wfpinf
138500020220     C                   z-add     orgcpf        wfpcpf
138600020220     C                   movel     orgloc        wfplof
138700020220     C                   movel     orgpro        wfpprf
138800020220     C                   movel     orgtel        wfptef
138900020220     C                   movel     orgfax        wfpfaf
139000001120     C                   update    wfptl002
139100001120     C                   endif
139200050404     C                   endif
139300050404     C*
139400050404     C* Se fornitore reperisco struttura commerciale tramite CAP/LOCALITA con TISI95R
139500050404     C                   if        PARCOF = 'F'
139600050404     C                   clear                   TISI95DS
139700050404     C                   eval      I95TCN = '3'
139800050404     C                   eval      I95CAP = wfpCAP
139900050404     C                   eval      I95PRV = wfpPRO
140000050404     C                   eval      I95LOC = wfpLOC
140100050404     C                   eval      I95NAR = wfpNAZ
140200050404     C                   call      'TISI95R'
140300050404     C                   parm                    TISI95DS
140400050404     C                   if        O95ERR = *blanks
140500050404     C                   z-add     O95LNA        depfil
140600050404     C     depfil        chain     azorg01l
140700050404     C                   if        %found(azorg01l)
140800050404     C                   z-add     orgfil        wfpfil
140900050404     C                   z-add     orgcar        wfpare
141000050404     C                   movel     orgfl3        wfpdiv
141100050404     C                   movel     orgdes        wfpdes
141200050404     C                   movel     orgind        wfpinf
141300050404     C                   z-add     orgcpf        wfpcpf
141400050404     C                   movel     orgloc        wfplof
141500050404     C                   movel     orgpro        wfpprf
141600050404     C                   movel     orgtel        wfptef
141700050404     C                   movel     orgfax        wfpfaf
141800050404     C                   update    wfptl002
141900050404     C                   endif
142000050404     C                   endif
142100050404     C                   endif
142200050404     C*
142300001120     C                   read(e)   wfptl02l
142400001120     C                   enddo
142500001120     C*
142600001120     C* Chiudo il file (WFPTL02L)
142700001120     C                   close     wfptl02l
142800001120     C*
142900001120     C                   ENDSR
143000000925     C*------------------------------------------------------------------------*
143100000925     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
143200000925     C*------------------------------------------------------------------------*
143300000925     C     CARORG        BEGSR
143400000925     C*
143500000925     C*---
143600000925     C* FILIALI O AGENZIE
143700000925     C*---
143800000925     C                   z-add     1             idx
143900000925     C*
144000000925     C     *loval        setll(e)  azorg01l
144100000925     C                   read(e)   azorg01l
144200000925     C                   dow       not(%eof)
144300000925     C                   if        (orgfag='A' or orgfag='F') and orgfva=*blanks
144400000925     C                   z-add     orgfil        FILAGE(idx)
144500000925     C                   add       1             idx
144600000925     C                   endif
144700000925     C                   read(e)   azorg01l
144800000925     C                   enddo
144900000925     C*
145000000925     C                   ENDSR
145100020923     C*--------------------------------------------------------------------------------------------*
145200020923     C* REPERISCE I DATI UTENTE
145300020923     C*--------------------------------------------------------------------------------------------*
145400020923     C     REPDATIUTE    BEGSR
145500020923     C*
145600020923     C* INIZIALIZZA VARIABILI DI WRK
145700020923     C                   CLEAR                   TIBS34DS
145800020923     C                   CLEAR                   AZUTEDS
145900020923     C                   CLEAR                   DDATIUTE
146000020923     C*
146100020923     C     *DTAARA       DEFINE    §azute        azuteds
146200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
146300020923     C                   IN(E)     *DTAARA
146400020923if  1C                   IF        %Error
146500020923     c                   EVAL      I34Tla = 'L'
146600020923     C                   CALL      'TIBS34R'
146700020923     C                   PARM                    Tibs34Ds
146800020923     C                   IN        *DTAARA
146900020923e   1C                   ENDIF
147000020923     C*
147100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
147200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
147300020923     C*
147400020923     C                   ENDSR
147500000000     C*------------------------------------------------------------------------*
147600000000     C* *INZSR - ROUTINE INIZIALE
147700000000     C*------------------------------------------------------------------------*
147800000000     C     *INZSR        BEGSR
147900000000     C*--------------------
148000000000     C* RICEVIMENTO PARAMETRI
148100000000     C*--------------------
148200000000     C     *ENTRY        PLIST
148300000000     C                   PARM                    KPJBA
148400000000     C                   MOVEL     KPJBU         PARAM
148500020923     C*
148600020923     C* REPERISCE I DATI DELL'UTENTE
148700020923     C                   EXSR      REPDATIUTE
148800020923     C                   MOVEL     DUTKCI        ALFAKCI           4
148900000000     C*--------------------
149000000000     C* CAMPI RIFERITI AL DATA BASE
149100000000     C*--------------------
149200050404     C     *LIKE         DEFINE    WFPSOC        KWPSOC
149300050404     C     *LIKE         DEFINE    WFPKSC        KWPKSC
149400000922     C     *LIKE         DEFINE    NTCAPL        KNTAPL
149500000922     C     *LIKE         DEFINE    NTCNK1        KNTNK1
149600000922     C     *LIKE         DEFINE    NTCNK2        KNTNK2
149700000922     C     *LIKE         DEFINE    NTCTNT        KNTTNT
149800000928     C     *LIKE         DEFINE    ACLSOCIETA    KBASOC
149900000928     C     *LIKE         DEFINE    ACLKCC        KBAKCC
150000000928     C     *LIKE         DEFINE    ACLKSC        KBAKSC
150100000922     C*
150200000922     C                   MOVEL(P)  'C'           KNTAPL
150300000922     C*
150400000000     C*--------------------
150500000000     C* CHIAVI DI LETTURA
150600000000     C*--------------------
150700050404     C*
150800050404     C* CHIAVE LETTURA WFPTL01L - COMPLETA
150900050404     C     KEYPTL01      KLIST
151000050404     C                   KFLD                    KWPSOC
151100050404     C                   KFLD                    KWPKSC
151200000922     C*
151300000922     C* CHIAVE LETTURA TFNTC01L - COMPLETA
151400050711     C     KEYNTC_P      KLIST
151500000922     C                   KFLD                    KNTAPL
151600000922     C                   KFLD                    KNTNK1
151700000922     C                   KFLD                    KNTNK2
151800050711     C*
151900050711     C* CHIAVE LETTURA TFNTC01L - COMPLETA
152000050711     C     KEYNTC        KLIST
152100050711     C                   KFLD                    KNTAPL
152200050711     C                   KFLD                    KNTNK1
152300050711     C                   KFLD                    KNTNK2
152400050711     C                   KFLD                    KNTTNT
152500000928     C*
152600000928     C* CHIAVE LETTURA YBACL01L - COMPLETA
152700000928     C     KEYYBA        KLIST
152800000928     C                   KFLD                    KBASOC
152900000928     C                   KFLD                    KBAKCC
153000000928     C                   KFLD                    KBAKSC
153100020220     C*
153200020220     C* CHIAVE LETTURA TABEL00F - COMPLETA
153300020220     C     KEYTAB        KLIST
153400020220     C                   KFLD                    TBLKUT
153500020220     C                   KFLD                    TBLCOD
153600020220     C                   KFLD                    TBLKEY
153700060202     C*
153800060202     C* CHIAVE LETTURA TNTBE01L - PARZIALE
153900060202     C     KEYTBE        KLIST
154000060202     C                   KFLD                    TBECOD
154100060202     C                   KFLD                    TBEKE1
154200060203     C                   KFLD                    TBEKE2
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
