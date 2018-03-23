000100170717     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200170717     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300170717     H DECEDIT('0,') DATEDIT(*DMY.)
000400170717      *
000500170717      *****************************************************************
000600170717      *
000700170717      *         ------------------------------------------------
000800170717      *                      Spesometro Trimestrale
000900170717      *                 Lancio aggiornamento stato record
001000170717      *         ------------------------------------------------
001100170717      *
001200170717R258  * Questa PGM è stato creato con il task R258
001300170717      *
001400971006     F*----------------------------------------------------*
001500170717     F* Comunicazione Spesometro Trimestrale
001600170717     FSDGCST07L UF   E           K DISK    RENAME(SDGCST000:SDGCST07)
001700170717     F                                     USROPN
001800170717     FSDGCST08L UF   E           K DISK    RENAME(SDGCST000:SDGCST08)
001900170717     F                                     USROPN
002000170717     FSDGCST09L UF   E           K DISK    RENAME(SDGCST000:SDGCST09)
002100170717     F                                     USROPN
002200170811     FSDGCSF01L UF   E           K DISK
002300170717     D*---------------------
002400170717     D SDGSI81DS     E DS
002500170717     D*---------------------
002600170717     D* Passaggio Parametri
002700170717     D KPJBA         E DS
002800170717     D*----------------
002900170717     D* Dati di ambiente ottenuti da SDGSOCDR
003000170717     D SOC001        E DS                  EXTNAME(SDGSOCDS )
003100170717     D*-------------
003200170717     D* DS Interna per dati di output di SDGSOCDR
003300170717     D XSOCDS          S           1000
003400170717     D*---------------------
003500971006     C*----------------------------------------------------*
003600971006     C*                MAIN LINE PROGRAM
003700971006     C*----------------------------------------------------*
003800971006     C*
003900971006     C* inizializzazione variabili
004000170719     C                   Exsr      InzVar
004100971006     C*
004200170717     C                   Exsr      AggCST
004300971006     C*
004400971006     C* fine programma
004500170719     C                   Exsr      EndPgm
004600971006     C/EJECT
004700971006     C************************************************************
004800971006     C* Fine Programma
004900971006     C************************************************************
005000170719     C     EndPgm        BegSr
005100971006     C*
005200971006     C                   SETON                                            LR
005300971006     C                   RETURN
005400971006     C*
005500170717     C                   EndSr
005600971006     C/EJECT
005700971006     C************************************************************
005800170717     C* AGGIORNAMENTO SDGCST
005900971006     C************************************************************
006000170717     C     AggCST        BegSr
006100971006     C*
006200170717     C                   Move      SocietaI81    CSTSocieta
006300170717     C*
006400170717     C                   Select
006500170717     C                   When      TpRecI81     = 'O'
006600170717     C                   Move      FileI81       CSTFileO
006700170717     C                   When      TpRecI81     = 'R'
006800170717     C                   Move      FileI81       CSTFileR
006900170717     C                   When      TpRecI81     = 'A'
007000170717     C                   Move      FileI81       CSTFileA
007100170717     C                   EndSl
007200170717     C*
007300170717     C                   Select
007400170717     C                   When      TpRecI81     = 'O'
007500170717     C     K02CST07      SetLL     SDGCST07L
007600170717     C     K02CST07      ReadE     SDGCST07L                              21
007700170717     C                   When      TpRecI81     = 'R'
007800170717     C     K02CST08      SetLL     SDGCST08L
007900170717     C     K02CST08      ReadE     SDGCST08L                              21
008000170717     C                   When      TpRecI81     = 'A'
008100170717     C     K02CST09      SetLL     SDGCST09L
008200170717     C     K02CST09      ReadE     SDGCST09L                              21
008300170717     C                   EndSl
008400170717     C*
008500170811     C* Aggiornamento catalogo
008600170811     C                   ExSr      UpdateCsf
008700170717     C*                                                         m
008800170717     C                   DoW       *In21 = *Off
008900170817     C                   eval      CSTDtUlMo   = %date()
009000170817     C                   eval      CSTUteUlMo  = XscCut
009100170717     C*
009200170717     C                   Select
009300170717     C                   When      TpRecI81     = 'O'
009400170717     C                   Move      IdFileI81     CSTIdFileO
009500170804     C                   Move      CSTPosizO     CSTIdPosO
009600170717     C                   Update    SDGCST07
009700170717     C                   When      TpRecI81     = 'R'
009800170717     C                   Move      IdFileI81     CSTIdFileR
009900170804     C                   Move      CSTPosizR     CSTIdPosR
010000170717     C                   Update    SDGCST08
010100170717     C                   When      TpRecI81     = 'A'
010200170717     C                   Move      IdFileI81     CSTIdFileA
010300170817     C                   Move      'A'           CSTTpRec
010400170717     C                   Update    SDGCST09
010500170717     C                   EndSl
010600170717     C*                                                         m
010700971007     C*
010800170717     C                   Select
010900170717     C                   When      TpRecI81     = 'O'
011000170717     C     K02CST07      ReadE     SDGCST07L                              21
011100170717     C                   When      TpRecI81     = 'R'
011200170717     C     K02CST08      ReadE     SDGCST08L                              21
011300170717     C                   When      TpRecI81     = 'A'
011400170717     C     K02CST09      ReadE     SDGCST09L                              21
011500170717     C                   EndSl
011600971007     C                   EndDo
011700971006     C*
011800170717     C                   EndSr
011900971006     C/EJECT
012000170811      ************************************************************
012100170811      * Aggiornamento catalogo
012200170811      ************************************************************
012300170811     C     UpdateCsf     BegSr
012400170811      *
012500170811     C                   eval      CsfSocieta = CstSocieta
012600170811     C                   eval      CsfFile    = FileI81
012700170811     C     K02Csf01      Chain     SdgCsf01L
012800170811     C                   If        %found(SdgCsf01l)   and
012900170811     C                             CSFIdFile  = *blank and
013000170811     C                             CSFDtInvio = *loval
013100170811      *
013200170811     C                   eval      CSFIdFile  = IdFileI81
013300170811     C                   eval      CSFDtInvio = %date()
013400170811      *
013500170817     C                   eval      CSFDtUlMo   = %date()
013600170817     C                   eval      CSFUteUlMo  = XscCut
013700170817      *
013800170811     C                   Update    SdgCsf000
013900170811     C                   EndIf
014000170811      *
014100170811     C                   EndSr
014200971006     C************************************************************
014300971006     C* OPERAZIONI INIZIALI
014400971006     C************************************************************
014500170717     C     *INZSR        BegSr
014600971006     C*
014700971006     C* Reperimento parametri
014800971006     C*
014900971006     C     *ENTRY        PLIST
015000971006     C                   PARM                    KPJBA
015100971006     C*
015200971006     C* Reperimento dati società
015300971006     C*
015400971006    >C                   MOVEL     'SOC001'      TIPXSC
015500971006     C                   MOVEL     *BLANK        SOCXSC
015600170717     C                   Exsr      REPSOC
015700971006     C*
015800170717     C                   EndSr
015900971006     C/EJECT
016000971006     C************************************************************
016100971006     C* INIZIALIZZAZIONE VARIABILI
016200971006     C************************************************************
016300170717     C     INZVAR        BegSr
016400170801     C*
016500170801     C                   MOVEL     KPJBU         SDGSI81DS
016600971006     C*
016700170717     C                   Select
016800170717     C                   When      TpRecI81     = 'O'
016900170717     C                   OPEN      SDGCST07L
017000170717     C                   When      TpRecI81     = 'R'
017100170717     C                   OPEN      SDGCST08L
017200170717     C                   When      TpRecI81     = 'A'
017300170717     C                   OPEN      SDGCST09L
017400170717     C                   EndSl
017500170717     C*
017600170717     C                   EndSr
017700971006     C/EJECT
017800170719     C************************************************************
017900170719     C* Reperimento dati società
018000170719     C************************************************************
018100170719     C     REPSOC        BEGSR
018200170719     C*
018300170719     C                   CALL      'SDGSOCDR'
018400170719     C                   PARM      'SOC001'      TIPXSC            6
018500170719     C                   PARM      SocietaI81    SOCXSC            3
018600170719     C                   PARM                    CDSXSC            9 0
018700170719     C                   PARM                    MODXSC            3
018800170719     C                   PARM      *BLANKS       RTNXSC            1
018900170719     C                   PARM                    XSOCDS
019000170719     C                   PARM                    KPJBA
019100170719     C*
019200170719     C                   If        RtnXsc     <> '1'
019300170719     C                   MoveL     XSOCDS        SOC001
019400170719     C                   ElSe
019500170719     C                   EXSR      ENDPGM
019600170719     C                   EndIf
019700170719     C*
019800170719     C                   ENDSR
019900170719     C/EJECT
020000971006     C************************************************************
020100971006     C* DEFINIZIONE KLIST
020200971006     C************************************************************
020300170717     C     DefKlist      BegSr
020400971006     C*
020500971006     C* klist
020600971006     C*
020700170717    >C* SDGCST07L
020800170717    >C     K02CST07      KLIST
020900170717    >C                   KFLD                    CSTSocieta
021000170717    >C                   KFLD                    CSTFileO
021100170717    >C* SDGCST08L
021200170717    >C     K02CST08      KLIST
021300170717    >C                   KFLD                    CSTSocieta
021400170717    >C                   KFLD                    CSTFileR
021500170717    >C* SDGCST09L
021600170717    >C     K02CST09      KLIST
021700170717    >C                   KFLD                    CSTSocieta
021800170717    >C                   KFLD                    CSTFileA
021900170811      *
022000170811     C     K02Csf01      KList
022100170811     C                   KFld                    CSFSocieta
022200170811     C                   KFld                    CSFFile
022300971006     C*
022400170717     C                   EndSr
022500170717     C************************************************************
