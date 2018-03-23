000100981210     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(PNOTA)
000200981210     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300000712     H DECEDIT('0,') DATEDIT(*DMY/)
000400960910     H*----------------------------------------------------------------
000500960910     H* NDCK32R - CONTABILIZZA INSOLUTI DA BANCA
000600960910     H*----------------------------------------------------------------
000700960910     H* LA CONTABILIZZAZIONE AGGIORNA:
000800960910     H*  1) LA TESTATA CON:
000900960910     H*     - RTKCONV = 'A'
001000960910     H*     - RTKATTI = 'A'
001100960910     H*  2) IL DETTAGLIO CON:
001200960910     H*     - RDKBA14 = 'A'
001300960910     H*----------------------------------------------------------------
001400960918     FNDRTK01L  UF   E           K DISK    UsrOpn
001500960911     F                                     COMMIT
001600960918     FNDRDK02L  UF   E           K DISK    UsrOpn
001700930601     F                                     COMMIT
001800960918     FDVPGB00F  O    E             DISK    UsrOpn
001900960911     F                                     COMMIT
002000960918     FNDEFA02L  UF   E           K DISK    UsrOpn
002100930601     F                                     COMMIT
002200960920     FNDINS00F  O    E             DISK    UsrOpn
002300960920     F                                     COMMIT
002400961105     FNDREG01L  IF   E           K DISK
002500970813     FNDPAS01L  IF   E           K DISK
002600961105     FANOPE01L  IF   E           K DISK
002700000502D0539FNDPPA01L  IF   E           K DISK
002800000502D0539FNDPPS01L  IF   E           K DISK
002900930503     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000921216     F                                     INFDS(CTRDS)
003100960911     D*----------
003200960911     D KPJBA         E DS
003300960911     D*----------
003400960916    >D SOC001        E DS                  EXTNAME(XSOC001DS)
003500981210     D    XscTesRid                   1    overlay(XscSkp:1)                    Scadenz. retroattivo
003600010503D0925D    XScNrDecMC                  1S 0 Overlay(XScSkP:6)
003700960916     D XSOCDS          DS          1000
003800960916     D*----------
003900000502     D* Parametro modulo insoluti
004000960911    >D NDCK37DS      E DS                  INZ
004100960911     D*----------
004200000502D0539D* Parametro modulo solleciti
004300000710a0812X*NDCL31DS      E DS                  INZ
004400000710a0812D ANP057DS      E DS                  INZ
004500000502     D*----------
004600960911     D XPAOUT          DS          2000
004700960911     D*----------
004800960911     D XATBDS        E DS
004900960911     D TABI07        E DS                  EXTNAME(ANGI07DS)
005000960918     D TAB1IN        E DS                  EXTNAME(ANG1INDS)
005100960911     D*----------
005200960913     D ANBRADS       E DS                  EXTNAME(ANBRA00F) INZ
005300961127     D ANBCODS       E DS                  EXTNAME(ANBCO00F) INZ
005400960913     D*----------
005500960913     D ND002DS       E DS
005600960916     D*----------
005700000502     D $Esito          S              1
005800981215     D NoBCO           S              1
005900960913     D $GesMsg         S              1
006000960913     D $LenOut         S              9B 0
006100960913     D $Struttura      S             10
006200960913     D $Kcc            S              6
006300960913     D $Ksc            S              8
006400960913     D $Soc            S              3
006500981215     D Output          S           4000
006600960916     D*----------
006700960916     D NDC075DS      E DS
006800960916     D NDC071DS      E DS
006900960918     D NDC050DS      E DS
007000960916     D*----------
007100970403     D* DS esterna per dati clienti
007200970403     D DVACLI        E DS                  INZ
007300970403     D*-------------
007400970403     D Dvaric          S              1
007500970403     D DvaSocieta      S              3
007600970403     D DvaUnita        S              8
007700970403     D DvaStrutt       S             10
007800970403     D DvaDtRif        S               D
007900970403     D DvaLenOut       S              9B 0
008000970403     D DvaSnatura      S              1
008100970403     D DvaCodice       S              8
008200970403     D DvaLineav       S              3
008300970403     D DvaFiliale      S              3  0
008400970403     D DvaSogg         S              8
008500970403     D DvaTpInd        S              2
008600970403     D DvaCdInd        S              3
008700970403     D DvaErrore       S              1
008800970403     D DvaOutput       S           4000
008900970403     D*-------------
009000960916     D STATUS         SDS           333
009100960916     D  DSPGM            *PROC
009200960916     D  PARMS            *PARMS
009300960916     D*----------
009400921216     D CTRDS           DS
009500921216     D  CTR                  367    368B 0
009600960911     D*----------
009700960918     D NrCarico        S                   Like(RtkCarN)
009800960911     D Oggi            S               D
009900960913     D DatMin          C                   CONST('0001-01-01')
010000960918     D DatMax          C                   CONST('9999-12-31')
010100960919     D Esito           S              1
010200960919     D Sys             S              3  0
010300960919     D NrAsReg         S              9  0
010400960919     D Prenot          S              1
010500961203     D DtReg10         S               D
010600961203     D DtSca10         S               D
010700000103     D WSerCtb         S                   Like(OPESer)
010800000114C1309D ZDateISO        S               D
010900010503D0925D*-------------
011000010503  "  D* Variabili per arrotondamento importo interessi
011100010503  "  D W3015           S             30S15
011200010503D0925D WNrDec          S              1S 0
011300960913     D*----------
011400000703D0638D* Prototipazione procedure EURO
011500000703  "  D/COPY *LIBL/SRCCPY,PJX002PR
011600000703  "  D*----------
011700000703  "  D WWDivisa        S                   Like(EFADivisa)
011800000703  "  D WWImporto       S                   Like(EFAImporto)
011900000703D0638D*----------
012000960920     C*--------------------------------------------------------------
012100960920     C* M A I N   L I N E   P R O G R A M
012200960920     C*--------------------------------------------------------------
012300960920     C*
012400960920     C* Testata ...
012500960918     C                   Z-Add     NrCarico      RtkCarN
012600960911     C     k02rtk01      Chain(N)  NDRTK000                           50
012700960912     C     *IN50         IFEQ      *OFF
012800960916     C                   Move      XScSoc        PgbSOC
012900961125     C                   Move      'CG'          PgbCTB
013000000630D0638X*****              Move      XScDUn        PgbUNT
013100961125     C                   Move      XScCut        PgbTRB
013200960916     C                   Move      KNRAZ         PgbRIF
013300960916     C                   Z-Add     KNRAZ         PgbNRJ
013400960916     C                   Move      'I'           PgbFLG
013500960916     C                   Move      NomPgm        PgbPGM
013600960916     C                   Move      KNMUS         PgbUTE
013700960916     C                   Move      Oggi          PgbDAT
013800961125     C                   Move      Oggi          PgbDRG
013900961125     C                   Move      PgbDRG        PgbDLG
014000961125     C                   Move      PgbDRG        PgbCDP
014100961125     C                   Move      PgbDRG        PgbCDA
014200000830D0690X*****              Z-Add     RtkCarN       PgbNRG
014300960920     C                   Move      DatMin        PgbDFL
014400960920     C                   Move      DatMin        PgbDOP
014500960920     C                   Move      DatMin        PgbDVA
014600930503     C                   ELSE
014700930503     C                   GOTO      FINE
014800960911     C                   ENDIF
014900960920     C*
015000020807D1181X****               MOVE      *BLANK        SAVRIF
015100020807D1181C                   MOVE      *HIVAL        SAVRIF
015200960920     C*
015300960920     C* Lettura dettaglio per Nr.Carico
015400960911     C     k02rdk02      SetLL     NDRDK000                           20
015500960911     C     *IN20         DOWEQ     *OFF
015600960919     C     k02rdk02      ReadE     NDRDK000                               20
015700960911     C     *IN20         IFEQ      *OFF
015800960920     C*
015900960920     C* Elaboro solo record con RDKBA14=*Blank (non contab./annullati)
016000960911     C     RDKBA14       IFEQ      ' '
016100960920     C*
016200000630     C* Rottura della contabile ...
016300960911     C     RDKOC51       IFNE      SAVRIF
016400960920     C*
016500960920     C* Registrazione AVERE di c/eff. in sofferenza
016600930115     C     TOTIMP        IFNE      0
016700960918     C                   EXSR      SB07
016800930503     C                   EXCEPT    SPAZIO
016900960911     C                   ENDIF
017000960920     C*
017100960920     C* Reperisco numero assoluto registrazione
017200960919     C                   Exsr      RepNrAsReg
017300960919     C                   Z-Add     NrAsReg       PgbNRA
017400960919     C                   Z-Add     NrAsReg       PgbPRG
017500960919     C                   Z-Add     Sys           PgbSYS
017600960919     C                   Z-Add     0             PgbNRR
017700960920     C*
017800960920     C* Azzero totali
017900921216     C                   Z-ADD     0             TOTIMP
018000000630D0638C                   Z-ADD     0             TOTIMPMC
018100921216     C                   Z-ADD     0             TOTSPE
018200000703D0638C                   Z-ADD     0             TOTSPEMC
018300961126     C                   Z-ADD     0             TOTSPEBAN
018400000703D0638C                   Z-ADD     0             TOTSPEBAMC
018500960911     C                   MOVEL     RDKOC51       SAVRIF
018600960920     C*
018700960920     C* Dettaglio
018800960918     C                   EXSR      DETINS
018900960920     C*
019000960920     C* ... No rottura
019100930115     C                   ELSE
019200960920     C*
019300960920     C* Dettaglio
019400960918     C                   EXSR      DETINS
019500960920     C*
019600960911     C                   ENDIF
019700960920     C*
019800960920     C* Aggiornamento dettaglio con flag di contabilizzato
019900960919     C                   MOVE      'A'           RDKBA14
020000960919     C                   UPDATE    NDRDK000
020100960920     C*
020200960920     C* Aggiornamento effetti (NdEfa)
020300960920     C                   Exsr      AggEffetti
020400960920     C*
020500960911     C                   ENDIF
020600960911     C                   ENDIF
020700960911     C                   ENDDO
020800960920     C*
020900960920     C* Ultimo insoluto letto:
021000960920     C* Registrazione AVERE di c/eff. in sofferenza
021100921216     C     TOTIMP        IFNE      0
021200960918     C                   EXSR      SB07
021300960911     C                   ENDIF
021400960920     C*
021500960920     C* Fine carico ----> Stampa totali
021600921216     C     FININS        IFNE      0
021700921216     C                   EXCEPT    TOTALE
021800960911     C                   ENDIF
021900960920     C*
022000960920     C* Aggiornamento testata (NdRtk) con flags di contabilizzato
022100960919     C     k02rtk01      CHAIN     NDRTK000                           50
022200960919     C     *IN50         IFEQ      *OFF
022300960919     C                   MOVE      'A'           RTKATTI
022400960919     C                   MOVE      'A'           RTKCONV
022500960919     C                   UPDATE    NDRTK000
022600960919     C                   ENDIF
022700960920     C*
022800960920     C* Contabilizzazione
022900960920     C                   Exsr      Contab
023000960920     C*
023100960920     C* Fine programma
023200921216     C     FINE          TAG
023300921216     C                   SETON                                        LR
023400930115     C*****************************************************
023500960917     C* DETTAGLIO INSOLUTO: per ogni effetto
023600930115     C*****************************************************
023700000630     C     DETINS        BEGSR
023800930115     C*
023900960911     C* Reperisco causali, banche, rapporti, operazioni
024000960918     C                   EXSR      SB06
024100930115     C*
024200960911     C* Gestione spese
024300960918     C                   EXSR      RSPESE
024400930115     C*
024500970813     C* Imposto campi
024600960918     C                   EXSR      SB01
024700970814     C*
024800970814     C* Cancellazione eventuale insoluto provvisorio
024900970825     C                   If        EfaNrAsReI <> 0
025000970814     C                   ExSr      CanInsP
025100970825     C                   EndIf
025200961121     C*
025300980303     C* Unità della registrazione:
025400980303     C* - Se contabile multiunità (ultimo char a dx del campo RDKOCOC è '*')
025500980303     C*    uso l'unità di default reperita dai parametri di elaborazione
025600980303     C* - Se contabile con una sola unità (ultimo char a dx del campo
025700000529     C*    RDKOCOC non è '*') uso l'unità reperita dalla registrazione
025800980303     C                   Move      RDKOCOC       $MultiUni         1
025900980303     C                   If        $MultiUni = '*'
026000980303     C                   Move      UnitaK37      PgbUNT
026100980303     C                   Else
026200980325     C                   Move      RDKUnita      PgbUNT
026300980303     C                   EndIf
026400930115     C*
026500970813     C* Effetto multipartita ...
026600970813     C                   If        EfaMulPar = *On
026700970813     C                   Move      *On           $NoPas            1
026800970813     C*
026900970813     C* Ciclo su NDPAS per suddividere l'effetto in righe diverse
027000970813     C                   Z-Add     EfaSys        PasSys
027100970813     C                   Z-Add     EfaNrAsReg    PasNrAsReg
027200970813     C                   Z-Add     1             PasNrRigaM
027300970813     C*
027400970813     C     k03pas01      SetLL     NdPas01L
027500970813     C     k03pas01      ReadE     NdPas01L                               23
027600970813     C                   DoW       *In23 = *Off
027700970813     C                   Move      *Off          $NoPas
027800970813     C*
027900000629D0638C* (le varie partite hanno tutte la stessa divisa dell'effetto)
028000000629  "  X*****              Z-Add     PasImportD    PgbImp
028100000629  "  C                   Z-Add     PasImportD    PgbIm2
028200000629D0638C                   Z-Add     PasImporto    PgbImp
028300970813     C                   Move      PasDtPar      PgbDRP
028400970813     C                   Z-Add     PasNrPar      PgbNRP
028500970813     C                   Move      PasSerPar     PgbSRP
028600990510     C* Segno
028700990510     C                   Move      'D'           PGBDA
028800990510     C                   If        PASDare = 1
028900990510     C                   Move      'A'           PGBDA
029000990510     C                   EndIf
029100990510     C*
029200970813     C* Dare cliente per importo capitale (effetto)
029300970813     C                   MOVE      TpRegzEK37    PgbTMO
029400970813     C                   Add       1             PgbNRR
029500970813     C                   WRITE     DVPGB000
029600970813     C*
029700970813     C     k03pas01      ReadE     NdPas01L                               23
029800970813     C                   EndDo
029900970813     C*
030000970813     C                   EndIf
030100970813     C*
030200970813     C* Effetto normale (o multipartita senza NDPAS) ...
030300970813     C                   If        EfaMulPar <> *On or
030400970813     C                             (EfaMulPar = *On and $NoPas = *On)
030500970812     C*
030600960917     C* Dare cliente per importo capitale (effetto)
030700960918     C                   MOVE      TpRegzEK37    PgbTMO
030800960919     C                   Add       1             PgbNRR
030900960911     C                   WRITE     DVPGB000
031000970813     C*
031100970813     C                   EndIf
031200930421     C*
031300960919     C* Scrittura file insoluti
031400960920     C                   Exsr      WrtInsol
031500960918     C*
031600970813     C* Stampa
031700960918     C                   EXSR      SBST1
031800960918     C*
031900930118     C                   ADD       1             FLGCLI
032000930421     C                   ADD       1             FININS            3 0
032100000703D0638X*****              ADD       RDKIMPO       FINIMP           19 3
032200000630D0638C                   ADD       EFAImporto    FINIMP           19 3
032300930115     C*
032400960917     C* Se le spese vanno addebitate al cliente
032500960917     C* Dare cliente per importo spese
032600930115     C     SPESE         IFEQ      'S'
032700960911     C     CLISPE        ANDGT     0
032800040322D1561C* e se da tabella primanota insoluti c'è addebito cliente='1'
032900040322D1561C     SCL1IN        ANDEQ     '1'
033000961128     C                   EXSR      SB04
033100960911     C                   ENDIF
033200930115     C*
033300960911     C* Totalizzo importi
033400961125     C                   ADD       RDKIMPO       TOTIMP           19 3
033500000630D0638C                   ADD       EFAImporto    TOTIMPMC         19 3
033600961125     C                   ADD       CLISPE        TOTSPE           19 3
033700000703D0638C                   ADD       CLISPEMC      TOTSPEMC         19 3
033800961126     C                   ADD       SpeBanca      TOTSPEBAN        19 3
033900000703D0638C                   ADD       SpeBancaMC    TOTSPEBAMC       19 3
034000930115     C*
034100930115     C                   ENDSR
034200960917     C******************************************************
034300960918     C* Prepara campi riga DARE cliente importo effetto
034400960917     C******************************************************
034500921216     C     SB01          BEGSR
034600930115     C*
034700930115     C                   MOVE      *BLANK        SPESE             1
034800930115     C*
034900960913     C                   Move      EfaKcc        PgbKCC
035000960913     C                   Move      EfaKsc        PgbKSC
035100930115     C*
035200000629D0638C* importo in divisa --> dall'effetto insoluto ricevuto
035300000703  "  C* importo m.c. --> dall'effetto originario
035400000629  "  X*****              Z-ADD     RDKIMPO       PgbIMP
035500000629  "  C                   Z-ADD     RDKIMPO       PgbIM2
035600000629D0638C                   Z-ADD     EFAImporto    PgbIMP
035700930115     C*
035800000103     C* Causale insoluto
035900000103     C* (se c'è ins. tecnico scrivo con causale apposita in riga cliente)
036000000103     C                   MOVE      CauI07        PgbCOT
036100960913     C                   MOVE      CauI07        PgbCOD
036200960913     C                   MOVE      CauI07        MOWCOD            4
036300960913     C     EfaInsTec     IFEQ      '1'
036400960918     C                   MOVE      CIT1IN        PgbCOD
036500960913     C                   ENDIF
036600000103     C* Serie registrazione da causale testata (se impostata)
036700000103     C                   If        WSerCtb <> *Blank
036800000103     C                   MOVE      WSerCtb       PgbSRG
036900000830D0690C                   Z-Add     0             PgbNRG
037000000103     C                   Else
037100000103     C                   Move      'HOBA'        PgbSRG
037200000830D0690C                   Z-Add     RtkCarN       PgbNRG
037300000103     C                   EndIf
037400930115     C*
037500960911     C* Trattamento spese per il cliente
037600960918     C     TraSpeK37     IFEQ      *On
037700930115     C     CLISPE        ANDGT     0
037800930115     C                   MOVE      'S'           SPESE
037900960911     C                   ENDIF
038000930115     C*
038100960913     C* Se insoluto tecnico ...
038200930115     C     SPESE         IFEQ      'S'
038300960913     C     EfaInsTec     ANDEQ     '1'
038400961125     C     AddSpITK37    ANDEQ     *Off
038500960917     C                   CLEAR                   CLISPE
038600000703D0638C                   CLEAR                   CLISPEMC
038700960911     C                   ENDIF
038800930115     C*
038900980122     C* Contropartita: banca c/c di addebito
039000980122     C                   MOVE      RTKBKCC       PgbCCP
039100980122     C                   MOVE      RTKBKSC       PgbSCP
039200930115     C*
039300960911     C                   MOVE      'D'           PgbDA
039400000629D0638X*****              Z-ADD     0             PgbIM2
039500960913     C                   MOVE      EfaDivisa     PgbDIV
039600960913     C                   Move      *Blank        PgbVOC
039700960913     C                   Move      *Blank        PgbRGRIPVA
039800960920     C                   Move      EfaDtSca      PgbDDC
039900960913     C                   Z-ADD     EfaNrEff      PgbNDC
040000000103     C                   Clear                   PgbSRN
040100960913     C                   Move      EfaDtPar      PgbDRP
040200960913     C                   Z-ADD     EfaNrPar      PgbNRP
040300960913     C                   Move      EfaSerPar     PgbSRP
040400960920     C                   Move      EfaDtSca      PgbDSC
040500960918     C                   MOVEL     DescrEK37     PgbDES
040600930118     C*
040700921216     C                   ENDSR
040800970814     C******************************************************
040900970814     C* Cancellazione eventuale insoluto provvisiorio
041000970814     C******************************************************
041100970814     C     CanInsP       BEGSR
041200970814     C*
041300970814     C                   Z-Add     EfaSysReI     RegSys
041400970814     C                   Z-Add     EfaNrAsReI    RegNrAsReg
041500970814     C     k02reg01      Chain     NdReg01L                           24
041600970814     C*
041700970814     C                   If        *In24 = *Off and
041800970814     C                             RegAnn <> *On and
041900970814     C                             RegTpRegZ = 'P'
042000970814     C*
042100970814     C* Annulla registrazione
042200970814     C                   CALLB     'NDMVC116'
042300970814     C                   PARM                    RegSys
042400970814     C                   PARM                    RegNrAsReg
042500970814     C                   PARM      '02'          Operaz            2
042600970814     C                   PARM      '0'           GesMsg            1
042700970814     C                   PARM                    Esito             1
042800970814     C                   PARM      '0'           Commit            1
042900970814     C*
043000970814     C                   EndIf
043100970814     C*
043200970814     C                   ENDSR
043300930115     C*****************************************************
043400960918     C* Dare di CLIENTE per importo spese (per ogni eff.)
043500930115     C*****************************************************
043600961128     C     SB04          BEGSR
043700930115     C*
043800000630D0638X*****              Z-ADD     CLISPE        PgbIMP
043900000630  "  C                   Z-ADD     CLISPE        PgbIM2
044000000703D0638C                   Z-ADD     CLISPEMC      PgbIMP
044100960918     C                   MOVE      CRS1IN        PgbCOD
044200960918     C                   MOVEL     DescrEK37     PgbDES
044300960918     C                   Move      VRS1IN        PgbVOC
044400960918     C                   Move      RRS1IN        PgbRGRIPVA
044500961125     C* Contropartita (c/to recupero spese)
044600960918     C                   Move      CRC1IN        PgbCCP
044700960918     C                   Move      SRS1IN        PgbSCP
044800000630D0599X*****              Move      EFADtSca      PgbDSC
044900000529D0599C                   Move      PgbDRG        PgbDSC
045000961125     C*
045100960918     C                   MOVE      TpRegzEK37    PgbTMO
045200960919     C                   Add       1             PgbNRR
045300960911     C                   WRITE     DVPGB000
045400930115     C*
045500930115     C                   ENDSR
045600930118     C*******************************************************
045700960917     C* ROTTURA DI NR.CARICO / RIFERIMENTO CONTABILE BANCA
045800930118     C*******************************************************
045900930118     C     SB07          BEGSR
046000930122     C*
046100980122     C* Imposto campi
046200980122     C                   EXSR      SB02
046300980122     C*
046400980122     C* Avere C/to Effetti in sofferenza (totale capitale)
046500980122     C* solo se richiesto due registrazioni separate
046600980122     C                   If        TpRegzBK37 <> 'U'
046700960913     C                   MOVE      MOWCOD        PgbCOD
046800960918     C                   MOVEL     DescrEK37     PgbDES
046900960918     C                   MOVE      TpRegzEK37    PgbTMO
047000960919     C                   Add       1             PgbNRR
047100960913     C                   WRITE     DVPGB000
047200980122     C                   EndIf
047300980122     C*
047400961126     C* Se ci sono spese effettivamente addebitate al cliente
047500960917     C* AVERE C/Recupero spese (solo importo spese)
047600970813     C     TOTSPE        IFGT      0
047700040322D1561C* e se da tabella primanota insoluti c'è addebito cliente='1'
047800040322D1561C     SCL1IN        ANDEQ     '1'
047900961128     C                   EXSR      SB05
048000960911     C                   ENDIF
048100960917     C*
048200980122     C* Se richiesta registrazione ...
048300960918     C     TpRegzBK37    IFEQ      'P'
048400980122     C     TpRegzBK37    OREQ      'U'
048500980122     C*
048600980122     C* DARE C/to Effetti in sofferenza
048700980122     C* solo se richiesto due registrazioni separate
048800980122     C                   If        TpRegzBK37 = 'P'
048900980122     C* Reperisco numero assoluto registrazione
049000960919     C                   Exsr      RepNrAsReg
049100960919     C                   Z-Add     NrAsReg       PgbNRA
049200960919     C                   Z-Add     NrAsReg       PgbPRG
049300960919     C                   Z-Add     Sys           PgbSYS
049400960919     C                   Z-Add     0             PgbNRR
049500961125     C                   MOVE      'P'           PgbTMO
049600961125     C                   MOVE      MOWCOD        PgbCOD
049700961125     C                   Clear                   PgbDES
049800961125     C                   MOVE      RTKEKCC       PgbKCC
049900961125     C                   MOVE      RTKEKSC       PgbKSC
050000961125     C                   MOVE      'D'           PgbDA
050100000630D0638X*****              Z-ADD     TOTIMP        PgbIMP
050200000630  "  C                   Z-ADD     TOTIMP        PgbIM2
050300000630D0638C                   Z-ADD     TOTIMPMC      PgbIMP
050400961125     C                   MOVE      RTKBKCC       PgbCCP
050500961125     C                   MOVE      RTKBKSC       PgbSCP
050600960919     C                   Add       1             PgbNRR
050700130226>>>>>C*                  WRITE     DVPGB000
050800980122     C                   EndIf
050900960920     C*
051000961125     C                   Move      DatMin        PgbDRP
051100961125     C                   Z-Add     0             PgbNRP
051200961125     C                   Move      *Blank        PgbSRP
051300961125     C*
051400961126     C     TOTSPEBAN     IFGT      0
051500960918     C     ModalBK37     ANDEQ     'S'
051600960920     C*
051700960920     C* DARE C/to Oneri bancari
051800961128     C                   EXSR      SB03
051900960919     C                   Add       1             PgbNRR
052000130226>>>>>C*                  WRITE     DVPGB000
052100960911     C                   ENDIF
052200960920     C*
052300960920     C* AVERE Banca C/C
052400961125     C                   EXSR      BANCACC
052500960919     C                   Add       1             PgbNRR
052600130226>>>>>C*                  WRITE     DVPGB000
052700960913     C                   ENDIF
052800960920     C*
052900930118     C                   ENDSR
053000930114     C***********************************************************
053100960917     C* IMPOSTAZIONE CAMPI REGISTR. - AVERE Effetti in sofferenza
053200930114     C***********************************************************
053300921216     C     SB02          BEGSR
053400930115     C*
053500960913     C                   Move      RTKEKCC       PgbKCC
053600960913     C                   Move      RTKEKSC       PgbKSC
053700930426     C*
053800000630D0638X*****              Z-ADD     TOTIMP        PgbIMP
053900000630  "  C                   Z-ADD     TOTIMP        PgbIM2
054000000630D0638C                   Z-ADD     TOTIMPMC      PgbIMP
054100930115     C*
054200960913     C                   MOVE      EfaKcc        PgbCCP
054300930118     C     FLGCLI        IFEQ      1
054400960913     C                   MOVE      EfaKsc        PgbSCP
054500930115     C                   ELSE
054600960923     C                   MOVE      *BLANK        PgbSCP
054700960911     C                   ENDIF
054800930115     C*
054900960911     C                   MOVE      'A'           PgbDA
055000960911     C                   Move      RTKCARD       PgbDRP
055100960911     C                   Z-ADD     RTKCARN       PgbNRP
055200000103     C                   Clear                   PgbSRP
055300961125     C                   MOVE      DatMin        PgbDSC
055400960916     C                   Move      RTKCARD       PgbDDC
055500960911     C                   Z-ADD     RTKCARN       PgbNDC
055600961125     C                   Clear                   PgbSRN
055700960911     C                   Move      *Blank        PgbVOC
055800960911     C                   Move      *Blank        PgbRGRIPVA
055900921216     C*
056000921216     C                   ENDSR
056100930118     C*****************************************************
056200960917     C* AVERE di RECUPERO SPESE
056300930118     C*****************************************************
056400961128     C     SB05          BEGSR
056500930118     C*
056600960918     C                   MOVE      CRC1IN        PgbKCC
056700960918     C                   MOVE      SRS1IN        PgbKSC
056800960918     C                   MOVE      CRS1IN        PgbCOD
056900960918     C                   MOVEL     DescrEK37     PgbDES
057000960918     C                   Move      VRS1IN        PgbVOC
057100960918     C                   Move      RRS1IN        PgbRGRIPVA
057200000630D0638X*****              Z-ADD     TOTSPE        PgbIMP
057300000630  "  C                   Z-ADD     TOTSPE        PgbIM2
057400000703D0638C                   Z-ADD     TOTSPEMC      PgbIMP
057500930118     C*
057600960913     C                   MOVE      EfaKcc        PgbCCP
057700930118     C     FLGCLI        IFEQ      1
057800960913     C                   MOVE      EfaKsc        PgbSCP
057900930118     C                   ELSE
058000960923     C                   MOVE      *BLANK        PgbSCP
058100960913     C                   ENDIF
058200930118     C*
058300960918     C                   MOVE      TpRegzEK37    PgbTMO
058400960919     C                   Add       1             PgbNRR
058500960913     C                   WRITE     DVPGB000
058600930118     C*
058700930118     C                   ENDSR
058800921216     C*****************************************************
058900961125     C* CAMPI REG. - DARE conto ONERI BANCARI -
059000921216     C*****************************************************
059100961128     C     SB03          BEGSR
059200921216     C*
059300960918     C                   MOVE      CSC1IN        PgbKCC
059400960918     C                   MOVE      SSD1IN        PgbKSC
059500961125     C                   MOVE      CSD1IN        PgbCOD
059600961125     C*
059700960913     C                   MOVE      'D'           PgbDA
059800000630D0638X*****              Z-ADD     TOTSPEBAN     PgbIMP
059900000630  "  C                   Z-ADD     TOTSPEBAN     PgbIM2
060000000703D0638C                   Z-ADD     TOTSPEBAMC    PgbIMP
060100921216     C*
060200960913     C                   MOVE      RTKBKCC       PgbCCP
060300960913     C                   MOVE      RTKBKSC       PgbSCP
060400960911     C*
060500960911     C* Prendo Voce/Regola dell'anagrafico se in tabella sono *blank
060600960918     C     VSD1IN        IFEQ      *Blank
060700960918     C     RSD1IN        ANDEQ     *Blank
060800960913     C                   MOVE      PgbKCC        $Kcc
060900960913     C                   MOVE      PgbKSC        $Ksc
061000960913     C                   EXSR      CallMvc002
061100960913     C     $Esito        IfEq      *Off
061200960913     C                   Move      Voce002       PgbVOC
061300960913     C                   Move      RgRipVa002    PgbRGRIPVA
061400960913     C                   EndIf
061500930118     C                   ELSE
061600960918     C                   Move      VSD1IN        PgbVOC
061700960918     C                   Move      RSD1IN        PgbRGRIPVA
061800960911     C                   ENDIF
061900930118     C*
062000921216     C                   ENDSR
062100921216     C*****************************************************
062200961125     C* CAMPI REG. - AVERE di BANCA C/C con TESORERIA -
062300921216     C*****************************************************
062400961125     C     BANCACC       BEGSR
062500921216     C*
062600960913     C                   MOVE      RTKBKCC       PgbKCC
062700960913     C                   MOVE      RTKBKSC       PgbKSC
062800961125     C                   MOVE      MOWCOD        PgbCOD
062900960913     C                   MOVE      'A'           PgbDA
063000000630D0638X*****              Z-ADD     TOTIMP        PgbIMP
063100000630  "  C                   Z-ADD     TOTIMP        PgbIM2
063200000630D0638C                   Z-ADD     TOTIMPMC      PgbIMP
063300980122     C* Per registrazioni "separate" ...
063400980122     C                   If        TpRegzBK37 = 'P'
063500961125     C                   MOVE      RTKEKCC       PgbCCP
063600961125     C                   MOVE      RTKEKSC       PgbSCP
063700980122     C* ... altrimenti (registrazione "unica")
063800980122     C                   Else
063900980122     C                   MOVE      EfaKcc        PgbCCP
064000980122     C     FLGCLI        IFEQ      1
064100980122     C                   MOVE      EfaKsc        PgbSCP
064200980122     C                   ELSE
064300980122     C                   MOVE      *BLANK        PgbSCP
064400980122     C                   ENDIF
064500980122     C                   EndIf
064600921216     C*
064700961125     C* Spese bancarie incluse in contabile oppure al netto
064800960918     C     ModalBK37     IFEQ      'S'
064900000630D0638X*****              ADD       TOTSPEBAN     PgbIMP
065000000630  "  C                   ADD       TOTSPEBAN     PgbIM2
065100000703D0638C                   ADD       TOTSPEBAMC    PgbIMP
065200960913     C                   ENDIF
065300960913     C*
065400960913     C                   Move      *Blank        PgbVOC
065500960913     C                   Move      *Blank        PgbRGRIPVA
065600921216     C*
065700960916     C                   MOVE      BRABanca      PgbBAN
065800960916     C                   MOVE      BRALinea      PgbLIN
065900981217     C                   MOVE      OpeCauBan     PgbCOP
066000921216     C*
066100960913     C     RTKOPED       IFNE      DatMin
066200960913     C                   Move      RTKOPED       PgbDOP
066300921216     C                   ELSE
066400960913     C                   Move      Oggi          PgbDOP
066500960913     C                   ENDIF
066600980529     C* Se data valuta media non è valorizzata uso data scadenza
066700980529     C     RDKVMED       IFNE      DatMin
066800980302     C                   Move      RDKVMED       PgbDVA
066900980529     C                   ELSE
067000980529     C                   Move      RDKDSCA       PgbDVA
067100980529     C                   ENDIF
067200921216     C*
067300921216     C                   ENDSR
067400930114     C*******************************************************
067500960911     C* REPERIMENTO CAUSALI, RAPPORTI, OPERAZIONI
067600930114     C*******************************************************
067700930114     C     SB06          BEGSR
067800930114     C*
067900000630D0638C                   Clear                   WSerCtb
068000000630     C*
068100960913     C                   MOVE      XSCCUT        XTBUTE
068200960913     C                   MOVE      '1'           XTBRIC
068300960913     C                   MOVE      XSCSOC        XTBAZI
068400960913     C                   MOVE      *ZERO         XTBKEY
068500960913     C                   MOVE      'I07'         XTBCOD
068600960913     C                   MOVE      RDKCAUS       XTBKEY
068700960923     C                   CALLb     'XATB'
068800960913     C                   PARM                    XATBDS
068900960913     C     XTBERR        IFEQ      *ON
069000960913     C                   CLEAR                   TABI07
069100960913     C                   ELSE
069200960913     C                   MOVEL     XTBUNI        TABI07
069300960913     C                   ENDIF
069400930115     C*
069500960913     C                   Move      'CG'          EfaCtb
069600960913     C                   Z-Add     RDKDSOR       EfaNrEff
069700960913     C     k03efa02      Chain(N)  NDEFA000                           50
069800960920     C   50              Clear                   NDEFA000
069900930115     C*
070000960913     C                   Eval      $Struttura = 'ANBRA'
070100960913     C                   Eval      $LenOut = %Size(ANBRADS)
070200960913     C                   Callb     'NDDVABRA'
070300960916     C                   PARM      '01'          $Operaz           2
070400960913     C                   PARM                    XScSoc
070500960913     C                   PARM                    *Omit
070600960913     C                   PARM                    *Omit
070700960913     C                   PARM                    RtkBKcc
070800960913     C                   PARM                    RtkBKsc
070900960913     C                   PARM                    $Struttura
071000960913     C                   PARM                    $LenOut
071100960913     C                   PARM                    $Esito
071200960913     C                   PARM                    ANBRADS
071300960913     C                   If        $Esito = *On
071400960913     C                   Clear                   ANBRADS
071500960913     C                   EndIf
071600960913     C*
071700980123     C                   Move      CauI07        OpeCausale
071800960913     C     k02ope01      CHAIN     ANOPE01L                           50
071900960913     C   50              Clear                   ANOPE000
072000000630     C  N50              Move      OPESer        WSerCtb
072100961127     C*
072200981215     C                   reset                   ANBCODS
072300961127     C                   Eval      $Struttura = 'ANBCO'
072400961127     C                   Eval      $LenOut = %Size(ANBCODS)
072500961127     C                   Callb     'NDDVABCO'
072600961127     C                   PARM      '01'          $Operaz           2
072700961127     C                   PARM                    XScSoc
072800961127     C                   PARM                    BraBanca
072900961127     C                   PARM                    BraLinea
073000961127     C                   PARM                    OpeCauBan
073100961127     C                   PARM                    Oggi
073200961127     C                   PARM                    $Struttura
073300961127     C                   PARM                    $LenOut
073400961127     C                   PARM                    $Esito
073500981215     C                   PARM                    Output
073600981215     C                   PARM                    XscTesRid
073700981215     C                   PARM                    NoBCO
073800981217     C* tutto Ok se è stato trovato ANBCO
073900981217     C                   If        $Esito = *Off and NoBco = *Off
074000981215     C                   MoveL     Output        ANBCODS
074100981215     C                   EndIf
074200930114     C*
074300930114     C                   ENDSR
074400930114     C*****************************************************
074500921216     C* SUBROUTINE STAMPA
074600921216     C*****************************************************
074700921216     C     SBST1         BEGSR
074800960920     C*
074900930503     C     ALFA          IFEQ      *BLANK
075000960920     C*
075100980529     C* Descrizione Conto Banca per intestazione tabulato
075200960920     C*
075300980529     C                   MOVE      RTKBKCC       $Kcc
075400980529     C                   MOVE      RTKBKSC       $Ksc
075500960913     C                   EXSR      CallMvc002
075600960913     C     $Esito        IfEq      *Off
075700960916     C                   MOVE      $Ksc          BANKSC            8
075800960913     C                   MOVEL     Descr002      BANRAG           44
075900960913     C                   EndIf
076000960920     C*
076100930503     C                   MOVE      'K'           ALFA              1
076200930503     C                   EXCEPT    TESTA
076300960920     C*
076400960913     C                   ENDIF
076500960920     C*
076600930503     C     CTR           IFGT      60
076700930503     C     *INOF         OREQ      '1'
076800921216     C                   EXCEPT    TESTA
076900960913     C                   ENDIF
077000921216     C*
077100960913     C* Descrizione del cliente
077200921216     C*
077300960913     C                   MOVE      PgbKCC        $Kcc
077400960913     C                   MOVE      PgbKSC        $Ksc
077500960913     C                   EXSR      CallMvc002
077600960913     C     $Esito        IfEq      *Off
077700960913     C                   MOVEL     Descr002      CLIRAG           44
077800960913     C                   EndIf
077900921216     C*
078000960913     C* Rovescio data scadenza
078100921216     C*
078200960916     C     PgbDSC        IfNe      DatMin
078300960916     C                   CALLb     'XDT4000'
078400960916     C                   PARM      PgbDSC        XDTAMG           10
078500960916     C     WWWDSC        PARM                    XDTGMA            6 0
078600960916     C                   PARM      2             XDTSTA            1 0
078700960916     C                   Else
078800960916     C                   Z-Add     0             WWWDSC            6 0
078900960916     C                   EndIf
079000921216     C*
079100960913     C* Rovescio data partita
079200930503     C*
079300960916     C     PgbDRP        IfNe      DatMin
079400960916     C                   CALLb     'XDT4000'
079500960916     C                   PARM      PgbDRP        XDTAMG           10
079600960916     C     WWWDRP        PARM                    XDTGMA            6 0
079700960916     C                   PARM      2             XDTSTA            1 0
079800960916     C                   Else
079900960916     C                   Z-Add     0             WWWDRP            6 0
080000960916     C                   EndIf
080100930503     C*
080200000703D0638X*****              Z-ADD     RDKIMPO       RBIMPW           19 3
080300000703D0638C                   Z-ADD     RDKIMPO       RBIMPW           18 3
080400930503     C*
080500930503     C                   EXCEPT    DETTAG
080600930503     C*
080700921216     C                   ENDSR
080800921216     C*****************************************************
080900921216     C* SUBROUTINE CALCOLO SPESE
081000921216     C*****************************************************
081100921216     C     RSPESE        BEGSR
081200921216     C*
081300961125     C                   Z-ADD     0             CLISPE           19 3
081400000703D0638C                   Z-ADD     0             CLISPEMC         19 3
081500000703  "  C*
081600000703  "  C* Calcola controvalore spese ricevute se divisa effetto <> da m.c.
081700000703  "  C                   If        EFADivisa <> XScDiv
081800000703  "  C                   Move      EFADivisa     WWDivisa
081900000703  "  C                   Z-Add     RDKImSp       WWImporto
082000000703  "  C                   ExSr      CalcImpMC
082100000703  "  C                   Z-Add     WWImporto     WDKImSpMC
082200000703  "  C                   Else
082300000703  "  C                   Z-Add     RDKImSp       WDKImSpMC        19 3
082400000703  "  C                   EndIf
082500000703D0638C*
082600961126     C* Spese da banca per totale oneri bancari
082700961126     C                   Z-ADD     RDKIMSP       SpeBanca         19 3
082800000703D0638C                   Z-ADD     WDKImSpMC     SpeBancaMC       19 3
082900000703  "  C*
083000000703  "  C* ATTENZIONE: se la divisa dell'effetto non è la m.c., nel calcolo
083100000630  "  C*  dell'ammontare delle spese del cliente sono considerate solo le
083200000630  "  C*  spese ricevute via HB perchè sicuramente sono congruenti (divisa)
083300000630D0638C*  con l'effetto, le altre ... chi lo sa ???
083400930115     C*
083500960911     C* Spese fisse
083600960918     C     SpeFisK37     IFEQ      *On
083700000630D0638C     EFADivisa     ANDEQ     XScDiv
083800960918     C                   ADD       ImpFisK37     CLISPE
083900000703D0638C                   ADD       ImpFisK37     CLISPEMC
084000960911     C                   ENDIF
084100930115     C*
084200961125     C* Spese da banca
084300960918     C     SpeNasK37     IFEQ      *On
084400961125     C                   ADD       RDKIMSP       CLISPE
084500000703D0638C                   ADD       WDKImSpMC     CLISPEMC
084600960911     C                   ENDIF
084700930115     C*
084800961125     C* Spese da condizioni bancarie
084900960918     C     SpeRapK37     IFEQ      *On
085000000630D0638C     EFADivisa     ANDEQ     XScDiv
085100040322D1561X* così si riimpostava il campo CLISPE  e non andava in aggiunta
085200040322  "  x* inoltre bisognava usare solo BCOImSpe2
085300040322  "  X**** BcoImSpes1    ADD       BcoImSpes2    CLISPE
085400040322D1561c                   ADD       BcoImspes2    Clispe
085500960916     C                   ADD       BcoImSpes3    CLISPE
085600040322D1561X**   BcoImSpes1    ADD       BcoImSpes2    CLISPEMC
085700040322D1561c                   ADD       BcoImspes2    ClispeMC
085800000703D0638C                   ADD       BcoImSpes3    CLISPEMC
085900960911     C                   ENDIF
086000921216     C*
086100921216     C                   ENDSR
086200000703D0638C*****************************************************
086300000703  "  C* Calcola importo in m.c.
086400000703  "  C*****************************************************
086500000703D0638C     CalcImpMC     BEGSR
086600000703     C*
086700000703     C                   Reset                   PJX00202DS
086800000703     C*
086900000703     C                   Move      WWDivisa      Divisa0202
087000000703     C                   Z-Add     WWImporto     ImporD0202
087100000703     C                   Move      XScDiv        DivMC0202
087200000712     C                   Move      DatMin        DtRif0202
087300000712     C                   Move      DatMin        DtRep0202
087400000703     C*
087500000703     C                   CallP     X0202TestC(PJX00202DS: %size(PJX00202DS):
087600000703     C                                        '0': Esito: *Omit)
087700000703     C*
087800000703     C                   If        Esito = *Off
087900000703     C                   Eval      WWImporto = ImporM0202
088000000703     C                   Else
088100000703     C                   Eval      WWImporto = 0
088200000703     C                   EndIf
088300000703     C*
088400000703D0638C                   ENDSR
088500930114     C*****************************************************
088600960919     C* Aggiornamento file effetti (NdEfa)
088700930114     C*****************************************************
088800960920     C     AggEffetti    BEGSR
088900960919     C*
089000960919     C* Aggiorno su effetti 'Nr.Ass.Reg.Ins.', 'Data Esito' e 'Flag Esito'
089100960913     C                   Move      'CG'          EfaCtb
089200960913     C                   Z-ADD     RDKDSOR       EfaNrEff
089300960913     C     k03efa02      CHAIN     NDEFA000                           50
089400960911     C     *IN50         IFEQ      *OFF
089500960919     C                   Z-Add     PgbNRA        EfaNrAsReI
089600960919     C                   Move      RtkCarD       EfaDtEsito
089700960913     C                   MOVE      XSCCUT        XTBUTE
089800960913     C                   MOVE      '1'           XTBRIC
089900960913     C                   MOVE      XSCSOC        XTBAZI
090000960913     C                   MOVE      *ZERO         XTBKEY
090100960913     C                   MOVE      'I07'         XTBCOD
090200960913     C                   MOVE      RDKCAUS       XTBKEY
090300960923     C                   CALLb     'XATB'
090400960913     C                   PARM                    XATBDS
090500960913     C     XTBERR        IFEQ      *OFF
090600960913     C                   MOVEL     XTBUNI        TABI07
090700960913     C                   Move      CauI07        OpeCausale
090800960913     C     k02ope01      CHAIN     ANOPE01L                           52
090900960911     C     *IN52         IFEQ      *OFF
091000960913     C     OpePortaf     IFEQ      '3'
091100960913     C                   MOVE      '4'           EfaEsito
091200960913     C                   ENDIF
091300960913     C     OpePortaf     IFEQ      '2'
091400960913     C                   MOVE      '1'           EfaEsito
091500960913     C                   ENDIF
091600960913     C                   ENDIF
091700960913     C                   ENDIF
091800960911     C                   UPDATE    NDEFA000
091900960913     C                   ENDIF
092000960918     C*
092100930114     C                   ENDSR
092200921216     C*****************************************************
092300960911     C* OPERAZIONI INIZIALI
092400921216     C*****************************************************
092500921221     C     *INZSR        BEGSR
092600921216     C*
092700000114C1309C     *DMY          Move      Udate         ZDateISO
092800000114C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
092900000114     C*
093000921216     C     *ENTRY        PLIST
093100921216     C                   PARM                    KPJBA
093200960911     C*
093300960918     C                   MOVEL     KPJBU         NrCarico
093400921216     C*
093500960911     C     k03efa02      KLIST
093600960911     C                   KFLD                    XScSoc
093700960911     C                   KFLD                    EfaCtb
093800960911     C                   KFLD                    EfaNrEff
093900960911     C*
094000960911     C     k02rtk01      KLIST
094100960911     C                   KFLD                    XScSoc
094200960918     C                   KFLD                    RtkCarN
094300921216     C*
094400960911     C     k02rdk02      KLIST
094500960911     C                   KFLD                    XScSoc
094600960918     C                   KFLD                    RtkCarN
094700961105     C*
094800961105     C     k02reg01      KLIST
094900961105     C                   KFLD                    RegSys
095000961105     C                   KFLD                    RegNrAsReg
095100970813     C*
095200970813     C     k03pas01      KLIST
095300970813     C                   KFLD                    PasSys
095400970813     C                   KFLD                    PasNrAsReg
095500970813     C                   KFLD                    PasNrRigaM
095600921216     C*
095700960913     C     k02ope01      KLIST
095800960913     C                   KFLD                    XScSoc
095900960913     C                   KFLD                    OpeCausale
096000000529D0539C* NDPPA01L
096100000502  "  C     K07PPA01      KLIST
096200000502  "  C                   KFLD                    PPASocieta
096300000502  "  C                   KFLD                    PPACtb
096400000502  "  C                   KFLD                    PPAKcc
096500000502  "  C                   KFLD                    PPAKsc
096600000502  "  C                   KFLD                    PPADtPar
096700000502  "  C                   KFLD                    PPANrPar
096800000502  "  C                   KFLD                    PPASerPar
096900000502  "  C* NDPPS01L
097000000502  "  C     K02PPS01      KLIST
097100000502  "  C                   KFLD                    PPSSys
097200000502D0539C                   KFLD                    PPSNrAsInt
097300970403     C*
097400970403     C* PLIST per NDDVASOG
097500970403     C     PDVASOG       PLIST
097600970403     C                   PARM                    DVARIC
097700970403     C                   PARM                    DVASOCIETA
097800970403     C                   PARM                    DVAUNITA
097900970403     C                   PARM                    DVASTRUTT
098000970403     C                   PARM                    DVADTRIF
098100970403     C                   PARM                    DVALENOUT
098200970403     C                   PARM                    DVASNATURA
098300970403     C                   PARM                    DVACODICE
098400970403     C                   PARM                    DVALINEAV
098500970403     C                   PARM                    DVAFILIALE
098600970403     C                   PARM                    *OMIT
098700970403     C                   PARM                    DVATPIND
098800970403     C                   PARM                    DVACDIND
098900970403     C                   PARM                    DVAERRORE
099000970403     C                   PARM                    DVAOUTPUT
099100960911     C*
099200960911     C     *LIKE         DEFINE    RDKOC51       SAVRIF
099300960911     C*
099400960911     C* Reperimento dati società
099500960911     C*
099600960911     C                   MOVEL     'SOC001'      TIPXSC
099700960911     C                   MOVEL     *BLANK        SOCXSC
099800960911     C                   EXSR      REPSOC
099900960911     C     RTNXSC        IFNE      '1'
100000960911     C                   MOVEL     XSOCDS        SOC001
100100960911     C                   ELSE
100200960911     C                   GOTO      FINE
100300960911     C                   ENDIF
100400960911     C*
100500960911     C* Reperimento parametri elaborazione insoluti (se esistono)
100600960911     C*
100700960911     C                   CALLB     'XPAR'
100800960911     C                   Parm                    XScSoc
100900960911     C                   Parm      'HBINRIBA'    XPaPar            8
101000960911     C                   Parm                    XPaOut
101100960911     C                   Parm      *OFF          XPaErr            1
101200960911     C                   Parm      ' '           XPaRic            1
101300960911     C     XPaErr        IfEq      *ON
101400960918     C                   Clear                   NdcK37Ds
101500960911     C                   Clear                   XPaOut
101600960911     C                   GOTO      FINE
101700960911     C                   Else
101800960918     C                   Movel     XPaOut        NdcK37ds
101900960911     C                   EndIf
102000000502D0539C*
102100000502 "   C* Reperimento parametri elaborazione SOLLECITI
102200000502 "   C*
102300000502 "   C                   CALLB     'XPAR'
102400000502 "   C                   Parm                    XScSoc
102500000502 "   C                   Parm      'NDCL1S  '    XPaPar            8
102600000502 "   C                   Parm                    XPaOut
102700000502 "   C                   Parm      *OFF          XPaErr            1
102800000502 "   C                   Parm      ' '           XPaRic            1
102900000710D0539C     XPaErr        IfEq      *ON
103000000710a0812X***                Clear                   NdcL31Ds
103100000710a0812C                   Clear                   Anp057Ds
103200000710D0539C                   Clear                   XPaOut
103300000502 "   C                   GOTO      FINE
103400000710d0539C                   Else
103500000710a0812X***                Movel     XPaOut        NdcL31ds
103600000710a0812C                   Movel     XPaOut        Anp057Ds
103700000502D0539C                   EndIf
103800960918     C*
103900960918     C* Tabella primanota insoluti
104000960918     C*
104100960918     C                   Eval      XTbRic = '1'
104200960918     C                   Eval      XTbAzi = XScSoc
104300960918     C                   Eval      XTbKey = *Zero
104400960918     C                   Eval      XTbCod = '1IN'
104500960918     C                   Move      TabPnIK37     XTbKey
104600960918     C                   CallB     'XATB'
104700960918     C                   Parm                    XAtbDS
104800960918     C                   If        XTbErr <> '0'
104900960918     C                   GOTO      FINE
105000960918     C                   Else
105100960918     C                   Eval      TAB1IN = XtbUni
105200960918     C                   EndIf
105300960916     C*
105400960916     C* Valorizzazione campi univoci testate
105500960916     C                   MOVEL     KNSIF         NOMSIF           10
105600960916     C                   MOVEL     XSCDSI        NOMDIT           20
105700960916     C                   MOVEL     DSPGM         NOMPGM           10
105800960916     C* Reperisco nome sistema informatico
105900960918     C                   CALL      'XNETA'                              21
106000960918     C                   PARM                    NOMSYS            8
106100960911     C*
106200960916     C                   TIME                    ORA               6 0
106300960911     C                   Z-ADD     66            CTR
106400960911     C     *DMY          Move      UDate         Oggi
106500960918     C                   Z-ADD     0             FININS
106600960918     C                   Z-ADD     0             FINIMP
106700960918     C                   Z-ADD     0             FLGCLI            4 0
106800960918     C*
106900960918     C* Attiva Commitment control
107000960918     C*
107100960918    >C                   CALLB     'XSTRCMT'
107200960918    >C                   OPEN      NdRtk01l
107300960918    >C                   OPEN      NdRdk02l
107400960918    >C                   OPEN      DvPgb00f
107500960920    >C                   OPEN      NdIns00f
107600960918    >C                   OPEN      NdEfa02l
107700960911     C*
107800921216     C                   ENDSR
107900960919     C******************************************************
108000960919     C* Scrittura file insoluti
108100960919     C******************************************************
108200960920     C     WrtInsol      BEGSR
108300961203     C*
108400961203     C                   Clear                   NdIns000
108500960919     C*
108600960920     C                   Move      PgbSOC        InsSocieta
108700980911     C                   Move      RDKUnita      InsUnita
108800960920     C                   Z-Add     PgbSYS        InsSys
108900960920     C                   Z-Add     PgbNRA        InsNrAsReg
109000960920     C                   Z-Add     PgbNRR        InsNrRigaM
109100960920     C                   Move      Oggi          InsDtImm
109200960920     C                   Move      PgbKCC        InsKcc
109300960920     C                   Move      PgbKSC        InsKsc
109400980316     C                   Move      EfaDtPar      InsDtPar
109500980316     C                   Z-Add     EfaNrPar      InsNrPar
109600980316     C                   Move      EfaSerPar     InsSerPar
109700961105     C                   Move      EfaMulPar     InsMulPar
109800960920     C                   Move      PgbCOD        InsCausale
109900960920     C                   Move      PgbCCP        InsKccCtp
110000960920     C                   Move      PgbSCP        InsKscCtp
110100961105     C                   Move      'E'           InsTipIns
110200961105     C                   Move      EfaDtEff      InsDtRif
110300961105     C                   Z-Add     EfaNrEff      InsNrRif
110400961105     C                   Move      EfaDtSca      InsDtSca
110500960920     C                   Z-Add     EfaNrAsReg    InsNrAsEff
110600960920     C                   Z-Add     EfaSys        InsSysEff
110700000630D0638X*****              Move      PgbDIV        InsDivisa
110800000630D0638C                   Move      EFADivisa     InsDivisa
110900981109     C                   Z-Add     EfaImportD    InsImpInsD
111000961105     C                   Z-Add     CLISPE        InsImpSpeD
111100040322D1561C* no, bisogna testare anche addebito spese cliente
111200040322  "  x*****              Move      TraSpeK37     InsAddSpe
111300040322  "  c* se si spese
111400040322  "  C                   if        Traspek37 = *on
111500040322  "  c* prendo valore tabella
111600040322  "  C                   move      Scl1in        InsAddSpe
111700040322  "  c                   else
111800040322  "  c                   Move      TraSpeK37     InsAddSpe
111900040322D1561C                   endif
112000961105     C                   Move      AIT1IN        InsAddInt
112100961105     C                   Move      RIE1IN        InsMdPagR
112200961105     C                   Move      ASP1IN        InsAddSpeR
112300961105     C                   Move      AIN1IN        InsAddIntR
112400961203     C                   Move      DatMin        InsDtScaR
112500961203     C                   Move      '1'           InsStatus
112600961203     C                   Move      '0'           InsStatusR
112700961105     C                   Move      RIT1IN        InsCRitor
112800961203     C                   Move      DatMin        InsDtCRit
112900960920     C                   Move      TabPnIK37     InsTabIns
113000961205     C* Insoluto tecnico --> No lettera
113100961205     C     EfaInsTec     IfEq      '1'
113200961205     C                   Move      *Off          InsLettera
113300961205     C                   Else
113400961205     C                   Move      LET1IN        InsLettera
113500961205     C                   ENDIF
113600961203     C* Calcolo interessi
113700961203     C                   Move      Oggi          DtReg10
113800961203     C                   Move      EfaDtSca      DtSca10
113900961203     C                   AddDur    IGA1IN:*D     DtReg10
114000961203     C     DtReg10       SubDur    DtSca10       GGRitardo:*D      3 0
114100010503D0925 *
114200010503  "   * L'importo degli interessi deve essere arrotondato al corretto
114300010503  "   * numero di decimali per la divisa, diversamente verrebbe scritto
114400010503  "   * sul file sempre con 3 decimali
114500010503  "   *
114600010503  "   * Reperisco numero decimali:
114700010503  "   * Se la divisa è diversa dalla moneta di conto, richiamo
114800010503  "   * X0201RpDvs per reperire il numero dei suoi decimali.
114900010503  "   * Se invece la divisa è la moneta di conto il  numero di
115000010503  "   * decimali è indicato sulla DS XSOC001DS
115100010503  "   * divisa <> m.c.
115200010503  "  C                   if        INSDivisa <> XScDiv
115300010503  "  C                   eval      WNrDec = *zero
115400010503  "  C                   if        X0201RpDVS (INSDivisa: INSDtImm:
115500010503  "  C                                         ANDVS00F: %size(ANDVS00F):
115600010503  "  C                                         ANDVX00F: %size(ANDVX00F)) = 0
115700010503  "  C                   eval      WNrDec = DVXNrDec
115800010503  "  C                   endif
115900010503  "   * divisa = m.c.
116000010503  "  C                   else
116100010503  "  C                   eval      WNrDec = XScNrDecMC
116200010503  "  C                   endif
116300010503D0925 *
116400961203     C                   Eval      InsImpIntD = InsImpInsD * GGRitardo *
116500961203     C                                          TAS1IN / 36000
116600010503D0925 *
116700010503  "   * Arrotondo l'importo degli interessi in divisa richiamando
116800010503  "   * X0201Arrot
116900010503  "  C                   eval      W3015 = INSImpIntD
117000010503  "  C                   callp     X0201Arrot (W3015 : 'H' : WNrDec)
117100010503  "  C                   eval      INSImpIntD = W3015
117200010503D0925 *
117300970403     C* Agente recupero crediti
117400000502D0539C* Se nel parametro modulo solleciti è specificato agente anagrafi
117500000710D0539C* co
117600000710a0812X***                if        L31Age = 'A'
117700000710a0812C                   if        GesAge057='A'
117800970403     C                   MOVE      '1'           DVARic
117900980325     C                   MOVE      RDKSocieta    DVASocieta
118000980325     C                   MOVE      RDKUnita      DVAUnita
118100970403     C                   MOVE      'C'           DVASNatura
118200970403     C                   MOVE      InsKsc        DVACodice
118300970403     C                   MOVEL     'DVACLI'      DVAStrutt
118400970403     C                   EVAL      DVALenOut = %Size(DVACLI)
118500970403     C                   CALLB     'NDDVASOG'    PDVASOG
118600970403   2 C                   If        DVAErrore <> *On
118700970403     C                   Eval      %Subst(DVACLI:1:DVALENOUT)
118800970403     C                              = %Subst(DVAOUTPUT:1:DVALENOUT)
118900970403     C                   Select
119000970403     C                   When      DVCAgente1 <> *Blank
119100970403     C                   Eval      InsKscAge = DVCAgente1
119200970403     C                   When      DVCAgente2 <> *Blank
119300970403     C                   Eval      InsKscAge = DVCAgente2
119400970403     C                   When      DVCAgente3 <> *Blank
119500970403     C                   Eval      InsKscAge = DVCAgente3
119600970403     C                   EndSl
119700970403     C                   EndIf
119800000502D0539c                   else
119900000502  "  c* altrimenti è l'agente di partita
120000000502  "  C                   MOVE      RdkSocieta    PPASocieta
120100000502  "  C                   MOVE      'CG'          PPACtb
120200000502  "  C                   MOVE      InsKcc        PPAKcc
120300000502  "  C                   MOVE      InsKsc        PPAKsc
120400000502  "  C                   Move      EfaDtPar      PpaDtPar
120500000502  "  C                   Z-Add     EfaNrPar      PpaNrPar
120600000502  "  C                   Move      EfaSerPar     PpaSerPar
120700000502  "  C     K07PPA01      chain     NDPPA000                           21
120800000502  "  C                   If        *in21 = *off
120900000502  "  c                   eval      ppssys = ppasys
121000000502  "  c                   eval      ppsnrasint = ppanrasint
121100000502  "  C     K02PPs01      chain     NDPPs000                           21
121200000502  "  C                   If        *in21 = *off
121300000502  "  c                   eval      inskscage = ppskscage
121400000502  "  c                   endif
121500000502  "  c                   endif
121600000502D0539c                   endif
121700970403     C*
121800960920     C                   Write     NdIns000
121900960919     C*
122000960919     C                   ENDSR
122100960919     C*---------------------------------------------------------*
122200960919     C* REPERISCO NUMERO ASSOLUTO                               *
122300960919     C*---------------------------------------------------------*
122400960919     C     REPNRASREG    BEGSR
122500960919     C*
122600960919     C                   CallB     'NDMVC101'
122700960919     C                   PARM                    Esito
122800960919     C                   PARM                    Sys
122900960919     C                   PARM                    NrAsReg
123000960919     C                   PARM      '1'           Prenot
123100960919     C*
123200960919     C                   ENDSR
123300960920     C*----------------------------------------------------*
123400960920     C* Contabilizzazione
123500960920     C*----------------------------------------------------*
123600960916     C     CONTAB        BEGSR
123700960920     C*
123800960920     C* Prepara ndbhp, ndbhm, ndbha
123900960916     C                   RESET                   NDC075DS
124000960916    >C                   Eval      Societa075 = XScSoc
124100960918    >C                   Eval      Rifint075 = PgbRif
124200960918    >C                   Eval      DtRegDa075 = DatMin
124300960918    >C                   Eval      DtRegA075 = DatMax
124400960916    >C                   Eval      SubNumD075 = 0
124500960916     C                   Eval      SubNumA075 = *Hival
124600960916     C                   Movel     Ndc075Ds      KPJBU
124700960920     C*
124800960916     C                   CALL      'NDC075R'
124900960916     C                   PARM                    KPJBA
125000960920     C*
125100960920     C* Scrive effettivamente movimenti
125200960916     C                   RESET                   NDC071DS
125300960916    >C                   Eval      Societa071 = XScSoc
125400960918    >C                   Eval      RifInt071 = PgbRif
125500960918    >C                   Move      DatMin        DtRegDa071
125600960919    >C                   Move      DatMax        DtRegA071
125700960916    >C                   Eval      SubNumD071 = 0
125800960916     C                   Eval      SubNumA071 = *Hival
125900960918    >C                   Eval      SolCont071 = *Off
126000960919    >C                   Eval      Interat071 = *Off
126100960916     C                   Movel     Ndc071Ds      KPJBU
126200960920     C*
126300960916     C                   CALL      'NDC071C'
126400960916     C                   PARM                    KPJBA
126500960918     C                   Movel     KPJBU         Ndc071Ds
126600960920     C*
126700960920     C* In caso di errore emette video con segnalazione:
126800960920     C* avviso il pgm chiamante che ci sono stati errori e gli do la
126900960920     C* possibilità di manutenzione p.n. batch in interattivo
127000960918     C                   If        Err071 = *On
127100960918     C                   Reset                   NDC050DS
127200960918     C                   Move      XScSoc        Societa050
127300960918     C                   Move      PgbRIF        RifInt050
127400960918     C                   Movel     Ndc050Ds      KPJBU
127500960918     C     Ret050        DoWNe     '2'
127600960918     C                   CALL      'NDC050R'
127700960918     C                   PARM                    KPJBA
127800960918     C                   Movel     KPJBU         Ndc050Ds
127900960918     C                   EndDo
128000960918     C                   EndIf
128100960920     C*
128200960916     C                   EndSr
128300960920     C************************************************************
128400960920     C* DECODIFICA CONTO
128500960920     C************************************************************
128600960913     C     CALLMVC002    BEGSR
128700960913     C*
128800960913     C* Decodifica conto
128900960913     C                   Clear                   ND002DS
129000960913     C                   EVAL      $LenOut = %Size(ND002DS)
129100960913     C                   CallB     'NDMVC002'
129200960913     C                   PARM      xscsoc        $soc
129300960913     C                   PARM                    $kcc
129400960913     C                   PARM                    $ksc
129500960913     C                   PARM                    *omit
129600960913     C                   PARM      *OFF          $GesMsg
129700960913     C                   PARM      *OFF          $Esito
129800960913     C                   PARM      'ND002DS'     $Struttura
129900960913     C                   PARM                    ND002DS
130000960913     C                   PARM                    $LenOut
130100960913     C*
130200960913     C                   ENDSR
130300960911     C************************************************************
130400960911     C* Reperimento dati società
130500960911     C************************************************************
130600960911     C     REPSOC        BEGSR
130700960911     C*
130800960911     C                   CALLB     'XSOC'
130900960911     C                   PARM                    TIPXSC            6
131000960911     C                   PARM                    SOCXSC            3
131100960911     C                   PARM                    CDSXSC            9 0
131200960911     C                   PARM                    MODXSC            3
131300960911     C                   PARM      *BLANKS       RTNXSC            1
131400960911     C                   PARM                    XSOCDS
131500960911     C                   PARM                    KPJBA
131600960911     C*
131700960911     C                   ENDSR
131800960911     O*----------------------------------------------------------------
131900921216     OQSYSPRT   E            TESTA            01
132000960911     O                       NOMDIT              21
132100960911     O                                           66 '* REGISTRAZIONE'
132200960916     O                                           77 'INSOLUTI *'
132300960911     O                       KNMUS              110
132400000114C1309O*****                  UDATE         Y    120
132500000114C1309O                       ZDate         Y    120
132600960911     O                                          126 'Pag.'
132700960911     O                       PAGE          Z    131
132800921216     O          E            TESTA            02
132900960918     O                       NOMSYS               9
133000960918     O                       NOMSIF              20
133100000712D0638X*****                                      68 'NUMERO AZIONE:'
133200000712D0638X*****                  KNRAZ             +  1
133300960911     O                       NOMPGM             110
133400960911     O                       ORA                120 '  :  :  '
133500921216     O          E            TESTA            03
133600000712D0638X*****                                      60 'REGISTRAZIONE N.:'
133700000712  "  X*****                  PgbNRG        Z   +  1
133800000712D0638X*****                                    +  1 'DEL'
133900000712C1309X*****                  UDATE         Y   +  1
134000000712D0638X***** ex C1309         ZDate         Y    120
134100960911     O                                          110 'gruppo PRO'
134200000712D0638O          E            TESTA            04
134300001215D0638O                                           17 'Registrazione nr.'
134400001218D0793X***** ex D0638         PgbNRG        Z
134500001215D0793O                       PgbNRG        Z   +  1
134600001215D0638O                                         +  1 'del'
134700000712D0638O                       ZDate         Y   +  1
134800930503     O          E            TESTA            05
134900000712D0638X*****                                       6 'Conto:'
135000000712D0638O                                            5 'Conto'
135100960916     O                       BANKSC            +  1
135200000712D0638X*****                  BANRAG            +  4
135300000712D0638O                       BANRAG            +  1
135400930503     O          E            TESTA       2  1
135500960918     O                                            8 'CLIENTE'
135600960918     O                                           69 'PARTITA'
135700960918     O                                           82 'SCADENZA'
135800960918     O                                           92 'EFFETTO'
135900000703D0638X*****                                     118 'IMPORTO'
136000000703  "  O                                          122 'IMPORTO DIV.'
136100000703  "  X*****                                     125 'CAUS.'
136200000703  "  O                                          127 'CAU.'
136300000703  "  X*****                                     131 'BANCA'
136400000703D0638O                                          132 'BAN.'
136500930503     O          E            DETTAG      1
136600960916     O                       PgbKSC               9
136700960918     O                       CLIRAG              54
136800960918     O                       WWWDRP        Y     63
136900960918     O                       PgbNRP        Z     73
137000960918     O                       WWWDSC        Y     82
137100980312     O                       EfaNrEff      Z     92
137200000703D0638X*****                  RBIMPW        2    118
137300000703  "  O                       RBIMPW        2    117
137400000703  "  O                       EFADivisa          122
137500000703  "  X*****                  PgbCOD             124
137600000703  "  O                       PgbCOD             127
137700000703  "  X*****                  BRABanca           130
137800000703D0638O                       BRABanca           132
137900930503     O          E            SPAZIO      1
138000921216     O          E            TOTALE      2
138100921216     O                                           23 'NR. INSOLUTI REGISTRATI'
138200921216     O                       FININS        Z     27
138300000703D0638X*****                                      91 'IMPORTO TOTALE'
138400000703D0638O                                           91 'IMPORTO TOTALE M.C.'
138500961125     O                       FINIMP        2    116
