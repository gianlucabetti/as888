000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400041015     FTNTBE01L  IF   E           K DISK
000500080129     FTABEL00F  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700080129     FTITAA30C  IF   E           K DISK
000800080129     FTIGCP51L  IF   E           K DISK
000900080130     FTIGNP01L  IF   E           K DISK
001000080130     FTIVGD00F  UF A E             DISK    commit
001100031103
001200031103     D*-----------
001300031103     D* SCHIERE A PROGRAMMA
001400031103     D*-----------
001500080128     D CAUBAR          S             10    DIM(110) CTDATA PERRCD(1)
001600080128     D CAUCLI          S             10    DIM(110) CTDATA PERRCD(1)
001700030924
001800041015     D*-----------
001900041015     D* VARIABILI D WRK
002000041015     D*-----------
002100080129     D prmesito        S              1
002200080129     D wrkesito        S                   like(prmesito)
002300080130     D j               S              2  0
002400080130     D savDS_KEYBOLLA  S                   like(wDS_KEYBOLLA) inz
002500041015
002600041015     D*-----------
002700041015     D* DEFINIZIONE DS ESTERNE
002800041015     D*-----------
002900080130     D wDS_SQLSTB    E DS                  extname(TISTB00F)
003000080130     D wDS_TIGCP     E DS                  extname(TIGCP00F)
003100080130     D wDS_TIGNP     E DS                  extname(TIGNP00F)
003200080213     D trul47ds      E DS
003300080129
003400080129     D*-----------
003500080129     D* DEFINIZIONE DS / SCHIERE CODICI EVENTO
003600080129     D*-----------
003700080129     D ds2a          e ds
003800080130     D sk2Alavv        s             10    dim(300)
003900080130     D sk2Agiac        s             10    dim(300)
004000080129     D i               s              3  0 inz
004100080129     D ii              s              3  0 inz
004200080128
004300080128     D*-----------
004400080128     D* VARIABILI RIFERITE AI DEFAULT CLIENTE
004500080128     D*-----------
004600080128     D wDftEvbLavv     S              2    inz('15')
004700080128     D wDftEvbGiac     S              2    inz('CG')
004800031103
004900030924     D*-----------
005000030924     D* RIDEFINIZIONE CHIAVE BOLLA IN FORMATO ALFA
005100030924     D*-----------
005200030924     D                 DS
005300080131     D  wSTBAAS                       4
005400080131     D  wSTBLNP                       3
005500080131     D  wSTBNRS                       2
005600080131     D  wSTBNSP                       7
005700080130     D wDS_NRSNSP              8     16
005800080130     D wDS_KEYBOLLA            1     16
005900031103
006000031103     D*-----------
006100031103     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
006200031103     D*-----------
006300031103     D                 DS
006400031103     D  TASAAS                 1      4  0
006500031103     D  TASMGS                 5      8  0
006600080130     D wDS_DATABOLLA           1      8
006700031103
006800031103     D*-----------
006900080131     D* DEFINIZIONE PARAMETRI X INDIRIZZAMENTO CLIENTE BASE DATI/DA ELABORARE
007000031103     D*-----------
007100080131 xxx D  wKSUlog        S              7  0 INZ(1020001)
007200080131     D  wKSUin         S              7  0 INZ(0450195)
007300080131 xxx D  wKSUout        S              7  0 INZ(1020001)
007400031103
007500031103     D*------------------
007600031103     D* DS "XSRDA8" - CONTROLLA DATA (8)
007700031103     D*------------------
007800031103     D WLBDA8          DS                  INZ
007900031103     D  G08DAT                 1      8  0
008000031103     D  G08INV                 9     16  0
008100031103     D  G08ERR                17     17
008200031103     D  G08TGI                18     22  0
008300080130
008400080130     D*------------------
008500080130     D* DS RMO "MIROGLIO"
008600080130     D*------------------
008700080130     D wDS_RMO         ds                  inz
008800080130     D  wRMO_TIPM                     1
008900080130     D  wRMO_CLIM                    11
009000080130     D  wRMO_TSER                     1
009100080130
009200080130     D*-----------
009300080130     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
009400080130     D*-----------
009500080130     D C_DS_Cli_TE     DS                                                       * File TESTATE
009600080130     D  CTDataElab                    8    INZ(*blanks)
009700080130     D  CTStsAvanz                    2    INZ(*blanks)
009800080130     D  CTDataSped                    8    INZ(*blanks)
009900080130     D  CTNrsSped10                  10    INZ(*blanks)
010000080130     D  CTTipDoc                      1    INZ('B')
010100080130     D  CTFilPart                     5    INZ(*blanks)
010200080130     D  CTFilArr                      5    INZ(*blanks)
010300080130     D  CTRagSocDest                 40    INZ(*blanks)
010400080130     D  CTIndDest                    35    INZ(*blanks)
010500080130     D  CTLocDest                    40    INZ(*blanks)
010600080130     D  CTCapDest                     5    INZ(*blanks)
010700080130     D  CTPrvDest                     2    INZ(*blanks)
010800080130     D  CTRagSocMitt                 40    INZ('MIROGLIO S.P.A. - VESTEBENE')
010900080130     D  CTIndMitt                    35    INZ('V.LE NOGARIS 20 - FR POLLENZO')
011000080130     D  CTLocMitt                    40    INZ('BRA')
011100080130     D  CTCapMitt                     5    INZ('12042')
011200080130     D  CTPrvMitt                     2    INZ('CN')
011300080130     D  CTColli                       5    INZ(*blanks)
011400080130     D  CTPesoBoll                    7    INZ(*blanks)
011500080130     D  CTPesoRil                     7    INZ(*blanks)
011600080130     D  CTVolumeBoll                  7    INZ(*blanks)
011700080130     D  CTTipMerce                    1    INZ(*blanks)
011800080130     D  CTTipMerceGP                  1    INZ(*blanks)
011900080130     D  CTDataCons                    8    INZ(*blanks)
012000080130     D  CTCodCli                     11    INZ(*blanks)
012100080130     D  CTTipSrv                      1    INZ(*blanks)
012200080130     D  CTFlag01                      1    INZ('N')
012300080130     D  CTValContr                    3    INZ('EUR')
012400080130     D  CTImpContr                   11    INZ(*blanks)
012500080130     D  CTLDV_Asseg                  10    INZ(*blanks)
012600080130     D  CTDataIncasso                 8    INZ(*blanks)
012700080130     D  CTDataGiac                    8    INZ(*blanks)
012800080130     D  CTRifMittAlfa                10    INZ(*blanks)
012900080130     D  CTRifMit_XX1                  5    INZ('99')
013000080130     D  CTRifMit_XX2                 15    INZ(*blanks)
013100080130     D  CTRifMit_XX3                  8    INZ(*blanks)
013200080130     D  CTRifMit_XX4                  7    INZ(*zeros)
013300080130     D  CTNote                      160    INZ('s.benevello@vestebene.com')
013400080130     D  CTFFD                         1    INZ(*blanks)
013500080130     D  CTNrsSped09                   9    INZ(*blanks)
013600080130     D*
013700080130     D C_DS_Cli_DE     DS                                                       * File DETTAGLI
013800080130     D  CDDataElab                    8    INZ(*blanks)
013900080130     D  CDNrsSped10                  10    INZ(*blanks)
014000080130     D  CDCodCli                     11    INZ(*blanks)
014100080130     D  CDDataVarSts                  8    INZ(*blanks)
014200080130     D  CDOraVarSts                   6    INZ(*blanks)
014300080130     D  CDDataCons                    8    INZ(*blanks)
014400080130     D  CDStsAvanz                    2    INZ(*blanks)
014500080130     D  CDRifMittAlfa                10    INZ(*blanks)
014600080130     D  CDNoteGiac                   80    INZ(*blanks)
014700080131     D  CDDataIncasso                 8    INZ(*zeros)
014800080131     D  CDDataGiac                    8    INZ(*zeros)
014900080130     D  CDNrsSped09                   9    INZ(*blanks)
015000080131     D  CD1SpedSvinc                  9    INZ(*zeros)
015100080131     D  CD2SpedSvinc                  9    INZ(*zeros)
015200080131     D  CDDestSvinc                   1    INZ(*blanks)
015300080131     D  CDNomeRicev                  20    INZ(*blanks)
015400080131     D  CDOraCons                     6    INZ(*blanks)
015500080130     D  CDPesoRil                     7    INZ(*blanks)
015600080130     D  CDVolumeRil                   7    INZ(*blanks)
015700080131     D  CDNumGiac                     7    INZ(*blanks)
015800080130     D  CDFille01                     2    INZ('XX')
015900080130
016000030924
016100030325
016200030924     C* MAIN/
016300080128     C*
016400080128     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016500080128     C
016600080128     C/EXEC SQL
016700080128     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
016800080128     C/END-EXEC
016900080213     C*
017000080213     C* Inizializzo variabili d wrk
017100080213     C                   movel     'N'           wProcediTE        1
017200080213     C                   movel     'N'           wProcediDE        1
017300080213     C*
017400080213     C* Avvio il blocco elaborazione TIVGD x tipo file: 'TE'
017500080213     C                   clear                   trul47ds
017600080213     C                   eval      d47opz  = 'I'
017700080213     C                   eval      d47tip  = 'TE'
017800080213     C                   eval      d47lck  = 'N'
017900080213     C                   eval      d47chkj = 'N'
018000080213     C                   eval      d47pgm  = 'TITV1D6R'
018100080213     C                   call      'TRUL47R'
018200080213     C                   parm                    trul47ds
018300080213     C*
018400080213     C* Se elaborazione consentita => proseguo
018500080213     C                   if        d47sts <> 'A'
018600080213     C                   movel     'S'           wProcediTE
018700080213     C                   endif
018800080213     C*
018900080213     C* Avvio il blocco elaborazione TIVGD x tipo file: 'DE'
019000080213     C                   clear                   trul47ds
019100080213     C                   eval      d47opz  = 'I'
019200080213     C                   eval      d47tip  = 'DE'
019300080213     C                   eval      d47lck  = 'N'
019400080213     C                   eval      d47chkj = 'N'
019500080213     C                   eval      d47pgm  = 'TITV1D6R'
019600080213     C                   call      'TRUL47R'
019700080213     C                   parm                    trul47ds
019800080213     C*
019900080213     C* Se elaborazione consentita => proseguo
020000080213     C                   if        d47sts <> 'A'
020100080213     C                   movel     'S'           wProcediDE
020200080213     C                   endif
020300080213     C*
020400080213     C* Se ok a procedere => elaboro
020500080213     C                   if        wProcediTE = 'S' AND
020600080213     C                             wProcediDE = 'S'
020700041015     C*
020800041015     C* Carica i dati tabellati
020900080129     C                   EXSR      cartab
021000041015     C*
021100041015     C* Esegue traduzione
021200991027     C                   EXSR      traduci
021300080131     C*
021400080131     C* Aggiorna log download "farlocchi"
021500080131     C                   EXSR      logdwl
021600080128     C*
021700080128     C* Se nn già diversamente valorizzato imposto qui l'esito a OK
021800080128     C                   IF        wrkesito = *blanks
021900080128     C                   EVAL      wrkesito = '1'
022000080128     C                   ENDIF
022100080213     C*
022200080213     C                   endif
022300080128     C*
022400080213     C* Infine eliminazione "blocco TRUL47"
022500080213     C                   EXSR      endTRUL47
022600080213     C*
022700921023     C                   SETON                                        LR
022800080213
022900080213
023000080213
023100080213
023200080213     C     endTRUL47     BEGSR
023300080213     C*
023400080213     C                   if        wProcediTE = 'S'
023500080213     C*
023600080213     C* Elimino il blocco elaborazione TIVGD x tipo file: 'TE'
023700080213     C                   clear                   trul47ds
023800080213     C                   eval      d47opz  = 'F'
023900080213     C                   eval      d47tip  = 'TE'
024000080213     C                   call      'TRUL47R'
024100080213     C                   parm                    trul47ds
024200080213     C*
024300080213     C                   endif
024400080213     C*
024500080213     C                   if        wProcediDE = 'S'
024600080213     C*
024700080213     C* Elimino il blocco elaborazione TIVGD x tipo file: 'DE'
024800080213     C                   clear                   trul47ds
024900080213     C                   eval      d47opz  = 'F'
025000080213     C                   eval      d47tip  = 'DE'
025100080213     C                   call      'TRUL47R'
025200080213     C                   parm                    trul47ds
025300080213     C*
025400080213     C                   endif
025500080213     C*
025600080213     C                   ENDSR
025700041015
025800080129
025900041015
026000041015
026100080129     C     cartab        BEGSR
026200080129     C*
026300080129     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
026400080129     C                   clear                   sk2Alavv
026500080129     C                   clear                   sk2Agiac
026600080129     C                   z-add     *zeros        i
026700080129     C                   z-add     *zeros        ii
026800080129     C                   eval      tblKUT = 1
026900080129     C                   eval      tblCOD = '2A'
027000080129     C     KEYtabP       setll     tabel00f
027100080129     C     KEYtabP       reade     tabel00f
027200080129     C                   dow       not %eof(tabel00f)
027300080129     C                   movel(p)  tblUNI        ds2a
027400080129     C                   if        §2aFTC = 'G'
027500080129     C                   add       1             i
027600080129     C                   eval      sk2Agiac(i) = tblKEY
027700080129     C                   endif
027800080129     C                   if        §2aFTC = 'A'
027900080129     C                   add       1             ii
028000080129     C                   eval      sk2Alavv(ii) = tblKEY
028100080129     C                   endif
028200080129     C     KEYtabP       reade     tabel00f
028300080129     C                   enddo
028400041015     C*
028500041015     C                   ENDSR
028600991027
028700080129
028800030924
028900030924
029000991027     C     traduci       BEGSR
029100080129     C*
029200080129     C/EXEC SQL
029300080129     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
029400080129     C/END-EXEC
029500080128     C*
029600080128     C/EXEC SQL
029700080128     C+ DECLARE C1 CURSOR FOR
029800080129     C+ SELECT TISTB00F.*,
029900080130     C+ concat(
030000080130     C+ concat(DIGITS(STBAAS),DIGITS(STBLNP)),
030100080130     C+ concat(DIGITS(STBNRS),DIGITS(STBNSP)))
030200080129     C+ FROM TISTB00F WHERE
030300080130     C+ concat(
030400080130     C+ concat(DIGITS(STBAAS),DIGITS(STBLNP)),
030500080130     C+ concat(DIGITS(STBNRS),DIGITS(STBNSP)))
030600080129     C+ IN (SELECT
030700080130     C+ concat(
030800080130     C+ concat(DIGITS(STBAAS),DIGITS(STBLNP)),
030900080130     C+ concat(DIGITS(STBNRS),DIGITS(STBNSP)))
031000080131     C+ FROM TISTB00F WHERE STBKSU = :wKSUin AND STBFTR = ' ')
031100080129     C+ ORDER BY STBAAS, STBLNP, STBNRS, STBNSP,
031200080129     C+ STBDAS, STBORS, STBDAOR
031300080129     C+ FOR UPDATE OF STBFTR
031400080128     C/END-EXEC
031500080128     C
031600080128     C/EXEC SQL
031700080128     C+ OPEN C1
031800080128     C/END-EXEC
031900080128     C
032000080128     C/EXEC SQL
032100080130     C+ FETCH C1 INTO :wDS_SQLSTB, :wDS_KEYBOLLA
032200080128     C/END-EXEC
032300080130     C*
032400080130     C* Valorizzo (purtroppo devo farlo a mano) i campi omologhi x le DS d wrk
032500080131     C                   if        sqlcod = *zeros
032600080131     C                   move(P)   stbAAS        wSTBAAS
032700080131     C                   move(P)   stbLNP        wSTBLNP
032800080131     C                   move(P)   stbNRS        wSTBNRS
032900080131     C                   move(P)   stbNSP        wSTBNSP
033000080131     C                   endif
033100080128     C*
033200080129     C                   dow       sqlcod = *zeros
033300080129     C*
033400080129     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
033500080130     C                   reset                   C_DS_Cli_TE
033600080130     C                   reset                   C_DS_Cli_DE
033700080130     C*
033800080130     C* Verifico la rottura d codice della chiave bolla
033900080130     C                   if        wDS_KEYBOLLA <> savDS_KEYBOLLA
034000080204     C*
034100080204     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO su tutti i record della bolla già elab.
034200080204     C                   exsr      updSqlSTB
034300080130     C*
034400080130     C* Ad ogni rottura d codice d bolla sancisco il commit
034500080130     C/EXEC SQL
034600080131     C+ COMMIT HOLD
034700080130     C/END-EXEC
034800080214     C*
034900080214     C                   commit
035000080130     C*
035100080130     C* Eseguo routine reperimento dati
035200080130     C                   exsr      rtvDati
035300080130     C*
035400080130     C* Eseguo routine interpretazione dati verticalizzati (TISTB)
035500080130     C                   exsr      exeSTB
035600080130     C*
035700080130     C* Eseguo routine scarico buffer file testata cliente
035800080130     C                   exsr      wriRec_TE
035900080130     C*
036000080130     C* Eseguo routine scarico buffer file dettaglio cliente
036100080130     C                   exsr      wriRec_DE
036200080130     C*
036300080130     C* Quindi salvo la chiave bolla corrente
036400080130     C                   eval      savDS_KEYBOLLA = wDS_KEYBOLLA
036500080130     C*
036600080130     C                   else
036700080130     C*
036800080130     C* Eseguo routine interpretazione dati verticalizzati (TISTB)
036900080130     C                   exsr      exeSTB
037000080130     C*
037100080130     C* Eseguo routine scarico buffer file dettaglio cliente
037200080130     C                   exsr      wriRec_DE
037300080130     C*
037400080130     C                   endif
037500080129     C*
037600080130     C* Proseguo con la lettura del recordset
037700080128     C*
037800080128     C/EXEC SQL
037900080130     C+ FETCH C1 INTO :wDS_SQLSTB, :wDS_KEYBOLLA
038000080128     C/END-EXEC
038100080130     C*
038200080130     C* Valorizzo (purtroppo devo farlo a mano) i campi omologhi x le DS d wrk
038300080131     C                   if        sqlcod = *zeros
038400080131     C                   move(P)   stbAAS        wSTBAAS
038500080131     C                   move(P)   stbLNP        wSTBLNP
038600080131     C                   move(P)   stbNRS        wSTBNRS
038700080131     C                   move(P)   stbNSP        wSTBNSP
038800080131     C                   endif
038900080130     C*
039000080128     C                   enddo
039100080204     C*
039200080204     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO su tutti i record della bolla già elab.
039300080204     C                   exsr      updSqlSTB
039400080204     C*
039500080204     C* Ad ogni rottura d codice d bolla sancisco il commit
039600080204     C/EXEC SQL
039700080204     C+ COMMIT HOLD
039800080204     C/END-EXEC
039900080214     C*
040000080214     C                   commit
040100080128     C*
040200080128     C/EXEC SQL
040300080130     C+ CLOSE C1
040400080128     C/END-EXEC
040500080130     C*
040600991027     C*
040700910830     C                   ENDSR
040800080130
040900080130
041000080130
041100080130     C     rtvDati       BEGSR
041200080130     C*
041300080130     C* Inizializzo le DS esterne e d quelle d wrk
041400080130     C                   clear                   wDS_RMO
041500080130     C                   clear                   wDS_TIGCP
041600080130     C                   clear                   wDS_TIGNP
041700080130     C*
041800080130     C* Innanzitutto chaino la bolla direttamente su TITAS
041900080130     C     KEYtas30P     chain     titas30c
042000080130     C                   if        %found(titas30c)
042100080130     C*
042200080130     C* Quindi aggancio l'estensione riferimenti bolla x RMO
042300080130     C                   eval      taaTRC = 'O'
042400080130     C     KEYtaa30      chain     titaa30c
042500080130     C                   if        %found(titaa30c)
042600080130     C                   eval      wDS_RMO = taaRSC
042700080130     C                   endif
042800080130     C*
042900080130     C* Quindi aggancio l'ultima giacenza
043000080130     C                   eval      gcpFRG = *zeros
043100080130     C     KEYgcp51      chain     tigcp51l
043200080130     C                   if        %found(tigcp51l)
043300080130     C* Se giacenza NN annullata la considero
043400080130     C                   if        gcpATB = *blanks
043500080130     C                   move      gcpDUR        CTDataGiac
043600080130     C                   move      gcpNGC        CDNumGiac
043700080130     C*
043800080130     C* Quindi aggancio e scorro le relative note d giacenza
043900080130     C     KEYgnp01P     setll     tignp01l
044000080130     C                   if        %found(tignp01l)
044100080130     C     KEYgnp01P     reade     tignp01l
044200080130     C                   dow       not %eof(tignp01l)
044300080130     C* Solo fasi 10 (ulteriore motivazione apertura giacenza)
044400080130     C                   if        gnpFAS = 10
044500080130     C                   eval      CDNoteGiac = %trim(CDNoteGiac)+%trim(gnpDMC)
044600080130     C                   endif
044700080130     C*
044800080130     C     KEYgnp01P     reade     tignp01l
044900080130     C                   enddo
045000080130     C                   endif
045100080130     C                   endif
045200080130     C                   endif
045300080130     C*
045400080130     C                   endif
045500080130     C*
045600080130     C                   ENDSR
045700080130     C***
045800080130
045900080130
046000080130
046100080130
046200080130     C     exeSTB        BEGSR
046300080130     C*
046400080130     C                   select
046500080130     C*
046600080130     C* Effettuo l'abbinamento forzato in caso d causare personalizzata 'INCARICO*'
046700080130     C                   when      stbTIS = '9'  AND
046800080130     C                             %subst(stbCOS:1:8) = 'INCARICO'
046900080130     C                   eval      CTStsAvanz = '10'
047000080130     C*
047100080130     C* Effettuo l'abbinamento forzato in caso d causare personalizzata 'CONSOK*'
047200080131     C                   when      stbTIS = '1'  AND
047300080131     C                             %subst(stbCOS:1:6) = 'CONSOK'
047400080130     C                   eval      CTStsAvanz = '75'
047500080130     C*
047600080130     C* Escludo tutti gli stati provenienti dai danni
047700080130     C                   when      stbPRS = '5'
047800080130     C*
047900080130     C* Gestisco i resi
048000080130 xxx C                   when      %subst(stbCOS:1:4) = 'RESO'
048100080130     C*
048200080130     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente Buffetti
048300080130     C                   other
048400080130     C                   if        stbTIS = '1'
048500080130     C*
048600080130     C* Gestisco particolarità causali
048700080130     C                   if        %trim(stbCOS) = '017' AND
048800080130     C                             (%subst(tasGVA:1:1) = 'P' OR
048900080130     C                              %subst(tasGVA:2:1) = 'P')
049000080130     C                   eval      CTStsAvanz = 'NH'
049100080130     C*
049200080130     C* Gestisco abbinamenti causali standard
049300080130     C                   else
049400080130     C                   z-add     1             j
049500080130     C     stbCOS        lookup    CAUBAR(j)                              55
049600080130     C                   if        %found
049700080130     C                   eval      CTStsAvanz = CAUCLI(j)
049800080130     C*
049900080130     C* Verifico se l'evento corrente (nn "esplicitato") trattasi d evento d tipo LAVV o GIAC
050000080130     C                   else
050100080130     C                   z-add     1             i
050200080130     C     stbCOS        lookup    sk2Agiac(i)                            55
050300080130     C                   if        %found
050400080130     C                   eval      CTStsAvanz = wDftEvbGiac
050500080130     C                   else
050600080130     C                   z-add     1             ii
050700080130     C     stbCOS        lookup    sk2Alavv(ii)                           55
050800080130     C                   if        %found
050900080130     C                   eval      CTStsAvanz = wDftEvbLavv
051000080130     C                   endif
051100080130     C                   endif
051200080130     C                   endif
051300080130     C                   endif
051400080130     C                   endif
051500080130     C*
051600080130     C                   endsl
051700080130     C*
051800080130     C* In ogni caso x uniformità attribuisco sempre e cmq lo stato d dettaglio = stato testata
051900080130     C                   eval      CDStsAvanz = CTStsAvanz
052000080130     C*
052100080130     C                   ENDSR
052200080130     C***
052300031103
052400080130
052500031103
052600031103
052700080130     C     wriRec_TE     BEGSR
052800031103     C*
052900080129     C                   EVAL      CTDataElab   =
053000080129     C                             %trim(%editw(datcor:'0        '))
053100080130     C                   EVAL      CTDataSped   = wDS_DATABOLLA
053200080130     C                   EVAL      CTNrsSped10  = wDS_NRSNSP
053300080129     C                   EVAL      CTFilPart    = %trim(%editw(TASLNP:'0   '))
053400080129     C                   EVAL      CTFilArr     = %trim(%editw(TASLNA:'0   '))
053500080129     C                   EVAL      CTRagSocDest = TASRSD
053600080129     C                   EVAL      CTIndDest    = TASIND
053700080129     C                   EVAL      CTLocDest    = TASLOD
053800080129     C                   EVAL      CTCapDest    = TASCAD
053900080129     C                   EVAL      CTPrvDest    = TASPRD
054000080129     C                   EVAL      CTColli      = %trim(%editw(TASNCL:'0     '))
054100080129     C                   EVAL      CTPesoBoll   =
054200080131     C                             %subst(%editw(TASPKB*100:'0         '):4:7)
054300080129     C                   IF        TASNCP = TASNCL
054400080129     C                   EVAL      CTPesoRil    =
054500080131     C                             %subst(%editw(TASPKC*100:'0         '):4:7)
054600080129     C                   ENDIF
054700080129     C                   EVAL      CTVolumeBoll =
054800080131     C                             '000'+%trim(%editw(TASVLB:'0     '))
054900080130     C                   EVAL      CTTipMerce   = wRMO_TIPM
055000080129     C                   EVAL      CTDataCons   =
055100080129     C                             %trim(%editw(TASDCM:'0        '))
055200080129     C                   EVAL      CTCodCli     = wRMO_CLIM
055300080129     C                   EVAL      CTTipSrv     = wRMO_TSER
055400080129     C                   EVAL      CTRifMittAlfa= STBRMA
055500080131     C                   IF        TASFFD = 'S'
055600080129     C                   EVAL      CTFFD        = TASFFD
055700080131     C                   ELSE
055800080131     C                   EVAL      CTFFD        = 'N'
055900080131     C                   ENDIF
056000080130     C                   EVAL      CTNrsSped09  = wDS_NRSNSP
056100080129     C*
056200080131     C* Se stato avanzamento valorizzato => Preparo la scrittura del file TIVGD
056300080131     C                   if        CTStsAvanz <> *blanks
056400080129     C                   clear                   tivgd000
056500080129     C                   eval      vgdtip = 'TE'
056600080129     C                   eval      vgdksu = *all'0'
056700080131     C                   move      wKSUout       vgdksu
056800080129     C                   eval      vgdtsc = 'WW'
056900080129     C                   eval      vgddat = datcor
057000080204     C                   eval      vgdpgm = 'TITV1D6C'
057100080129     C*
057200080404     C                   eval      VGDDTA = %subst(C_DS_Cli_TE:1:
057300080404     C                                      %size(C_DS_Cli_TE))
057400080129     C*
057500080129     C                   write     tivgd000
057600080131     C                   endif
057700080129     C*
057800080129     C                   ENDSR
057900080128
058000080128
058100080128
058200080128
058300080130     C     wriRec_DE     BEGSR
058400080129     C*
058500080129     C                   EVAL      CDDataElab    =
058600080129     C                             %trim(%editw(datcor:'0        '))
058700080130     C                   EVAL      CDNrsSped10   = wDS_NRSNSP
058800080129     C                   EVAL      CDCodCli      = wRMO_CLIM
058900080129     C                   EVAL      CDDataVarSts  =
059000080129     C                             %trim(%editw(STBDAS:'0        '))
059100080129     C                   EVAL      CDOraVarSts   =
059200080131     C                             %trim(%editw(STBORS*100:'0      '))
059300080129     C                   EVAL      CDDataCons    =
059400080129     C                             %trim(%editw(TASDCM:'0        '))
059500080129     C                   EVAL      CDRifMittAlfa = STBRMA
059600080130     C                   EVAL      CDNrsSped09   = wDS_NRSNSP
059700080129     C                   EVAL      CDOraCons     =
059800080131     C                             %trim(%editw(TASHMC*100:'0      '))
059900080129     C                   IF        TASNCP = TASNCL
060000080129     C                   EVAL      CDPesoRil    =
060100080131     C                             %subst(%editw(TASPKC*100:'0         '):4:7)
060200080129     C                   ENDIF
060300080129     C                   IF        TASNCR = TASNCL
060400080129     C                   EVAL      CDVolumeRil   =
060500080131     C                             '000'+%trim(%editw(TASVLC:'0     '))
060600080129     C                   ENDIF
060700080131     C*
060800080131     C* Se stato avanzamento valorizzato => Preparo la scrittura del file TIVGD
060900080131     C                   if        CDStsAvanz <> *blanks
061000080128     C                   clear                   tivgd000
061100080129     C                   eval      vgdtip = 'DE'
061200080129     C                   eval      vgdksu = *all'0'
061300080131     C                   move      wKSUout       vgdksu
061400080129     C                   eval      vgdtsc = 'WW'
061500080129     C                   eval      vgddat = datcor
061600080204     C                   eval      vgdpgm = 'TITV1D6C'
061700080128     C*
061800080404     C                   eval      VGDDTA = %subst(C_DS_Cli_DE:1:
061900080404     C                                      %size(C_DS_Cli_DE))
062000080128     C*
062100080128     C                   write     tivgd000
062200080131     C                   endif
062300080128     C*
062400080128     C                   ENDSR
062500080129
062600080129
062700080129
062800080129
062900080130     C     updSqlSTB     BEGSR
063000080129     C*
063100080129     C* Aggiorno il record corrente del recordset come trasmesso
063200080129     C*
063300080129     C/EXEC SQL
063400080204     C+  UPDATE TISTB00F
063500080204     C+  SET    STBFTR = 'T'
063600080204     C+  WHERE
063700080204     C+  concat(
063800080204     C+  concat(DIGITS(STBAAS),DIGITS(STBLNP)),
063900080204     C+  concat(DIGITS(STBNRS),DIGITS(STBNSP))) =
064000080204     C+  :savDS_KEYBOLLA
064100080129     C/END-EXEC
064200080129     C*
064300080129     C                   ENDSR
064400080129     C***
064500080131
064600080131
064700080131
064800080131
064900080131     C     logDwl        BEGSR
065000080131     C*
065100080131     C/EXEC SQL
065200080131     C+  update tivlt00f set vltsts = '1'
065300080131     C+  where vltksc = :wKSUlog and vlttip in ('BT', 'VC')
065400080131     C+  and vltsts = '0'
065500080131     C/END-EXEC
065600080131     C*
065700080131     C                   ENDSR
065800080131     C***
065900080128
066000080128
066100080128
066200080128
066300080128     C     *pssr         BEGSR
066400080128     C*
066500080130     C                   eval      wrkesito = '2'
066600080128     C*
066700080128     C                   seton                                        LR
066800080128     C*
066900080128     C                   ENDSR     '*CANCL'
067000080128     C***
067100080128
067200031103
067300031103
067400991027
067500991027      /TITLE Operazioni iniziali.
067600991027     C     *inzsr        BEGSR
067700991027     C*
067800991027     C     *ENTRY        PLIST
067900991027     C     wrkesito      parm      wrkesito      prmesito
068000031103     C*
068100031103     C* Calcola la data corrente
068200031103     C                   time                    wn14             14 0
068300031103     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068400031103     C                   z-add     wn8           g08dat
068500031103     C                   z-add     *zeros        g08inv
068600031103     C                   movel     '0'           g08err
068700031103     C                   call      'XSRDA8'
068800031103     C                   parm                    wlbda8
068900031103     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069000030709     C*
069100030709     C* Definizione chiavi
069200031103     C*
069300031103     C* Chiave su TITAS30C - Parziale
069400031103     C     KEYtas30P     KLIST
069500031103     C                   KFLD                    stbAAS
069600031103     C                   KFLD                    stbLNP
069700031103     C                   KFLD                    stbNRS
069800031103     C                   KFLD                    stbNSP
069900080129     C*
070000080129     C* Chiave su TIGCP51L - Completa
070100080129     C     KEYgcp51      KLIST
070200080129     C                   KFLD                    stbAAS
070300080129     C                   KFLD                    stbLNP
070400080129     C                   KFLD                    stbNRS
070500080129     C                   KFLD                    stbNSP
070600080129     C                   KFLD                    gcpFRG
070700080130     C*
070800080130     C* Chiave su TIGNP01L - Parziale
070900080130     C     KEYgnp01P     KLIST
071000080130     C                   KFLD                    gcpAGC
071100080130     C                   KFLD                    gcpFGC
071200080130     C                   KFLD                    gcpNGC
071300080130     C                   KFLD                    gcpFRG
071400080129     C*
071500080129     C* Chiave su TITAA30C - Completa
071600080129     C     KEYtaa30      KLIST
071700080129     C                   KFLD                    stbAAS
071800080129     C                   KFLD                    stbLNP
071900080129     C                   KFLD                    stbNRS
072000080129     C                   KFLD                    stbNSP
072100080129     C                   KFLD                    taaTRC
072200080129     C*
072300080129     C* Chiave su TABEL00F - parziale
072400080129     C     KEYtabP       KLIST
072500080129     C                   KFLD                    tblKUT
072600080129     C                   KFLD                    tblCOD
072700991027     C*
072800991027     C                   ENDSR
072900031103** CAUBAR - CAUSALI BARTOLINI
073000080130
073100080130
073200080130
073300080128A16
073400080128016
073500080128026
073600080128MIC
073700080128RIC
073800080130
073900080130
074000080128DIR
074100080130
074200080130
074300080130
074400080130
074500080130
074600080130
074700080130
074800080130
074900080130
075000080128017
075100080130
075200080130
075300080130
075400080130
075500080130
075600080130
075700080130
075800080128030
075900080130
076000080128P
076100080130
076200080130
076300080130
076400080130
076500080130
076600080130
076700080128ZMP
076800080128ZBS
076900080128ZSC
077000080128008
077100080128013
077200080128014
077300080130
077400080130
077500080130
077600080130
077700080128ZNV
077800080128ZMM
077900080128ZAL
078000080128034
078100080130
078200080130
078300080130
078400080128035
078500080130
078600080130
078700080130
078800080130
078900080130
079000080128DAN
079100080128RFD
079200080128RFM
079300080130
079400080130
079500080130
079600080130
079700080130
079800080130
079900080128007
080000080128002
080100080128006
080200080130
080300080130
080400080130
080500080128003
080600080128005
080700080128012
080800080130
080900080130
081000080128032
081100080130
081200080128022
081300080130
081400080130
081500080128004
081600080128018
081700080128027
081800080128030
081900080128T
082000080128A23
082100080128023
082200080128024
082300080128025
082400080130
082500080130
082600080130
082700080128019
082800080128PAT
082900080128020
083000080130017
083100080130
083200080130
083300080130
083400080130
083500080128021
083600080128
083700080128
083800080128
083900080128
084000080128** CAUCLI - CAUSALI CLIENTE BUFFETTI
08410008013010
08420008012811
08430008012812
08440008012813
08450008012813
08460008012813
08470008012814
08480008012814
08490008013015
08500008012816
08510008012817
08520008012820
08530008012829
08540008012830
08550008012831
08560008012832
08570008012833
08580008012834
08590008012864
08600008012865
08610008012870
08620008013075
08630008012879
08640008012880
08650008012885
08660008012887
08670008012889
08680008012890
08690008012893
08700008012895
08710008012896
08720008012897
08730008012898
08740008012899
08750008012800
08760008012807
087700080128AI
087800080128AN
087900080128AN
088000080128AN
088100080128AP
088200080128AP
088300080128AP
088400080128AT
088500080128AZ
088600080128BK
088700080128BS
088800080128BW
088900080128BW
089000080128BW
089100080128BW
089200080130CG
089300080128CI
089400080128CL
089500080128CR
089600080128CX
089700080128DB
089800080128DF
089900080128DI
090000080128DN
090100080128DR
090200080128DR
090300080128DR
090400080128DS
090500080128DW
090600080128EA
090700080128EE
090800080128EM
090900080128EQ
091000080128EU
091100080128FD
091200080128FH
091300080128FL
091400080128FT
091500080128GD
091600080128GK
091700080128GS
091800080128HB
091900080128HJ
092000080128HO
092100080128HP
092200080128HR
092300080128IA
092400080128IE
092500080128JU
092600080128JY
092700080128JY
092800080128JY
092900080128JY
093000080128KX
093100080128KX
093200080128KX
093300080128KX
093400080128KX
093500080128LA
093600080128LB
093700080128LF
093800080128LW
093900080128ME
094000080128MV
094100080130NH
094200080128PS
094300080128PX
094400080128RL
094500080128SL
094600080128WA
094700080128
094800080128
094900080128
095000080128
