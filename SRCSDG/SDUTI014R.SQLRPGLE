000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) DYNUSRPRF(*OWNER)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*
000500000000     H*               -------------------------------
000600170706     H* Generazione tabella HD4 da tabella G12
000700000000     H*
000800000000     H*
000900000000     F*----------------------------------------------------*
001000000000     FQADSPFFD  IF   E             DISK    USROPN
001100000000     D*-------------
001200000000     D Ncampi          S                   DIM(100) Like(WHFLDI) inz            X QCMDEXC
001300000000     D CM1             S             80    DIM(3) CTDATA PERRCD(1)              X QCMDEXC
001800000000     D*------------------
001900000000     D* DS Esterna gestione messaggi
002000000000     D SDGMSGDS      E DS
002100000000     D  STS          E                     EXTFLD(MSGSTS)
002200000000     D                                     DIM(333)
002300000000     D  JBA          E                     EXTFLD(MSGJBA)
002400000000     D                                     DIM(502)
002401170707     D* Passaggio Parametri
002402170707     D KPJBA         E DS
002403170706     D*-------------
002404170706     D* Dati ottenuti da XSOC
002405170706     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002406170706     D*-------------
002407170706     D* DS Interna per dati di output di SDGSOCDR
002408170706     D XSOCDS          DS          1000
002409170706      *-------------
002410170706     D* Reperimento nome PGM
002411170706     D STATUS         SDS           333
002412170706     D  DSPGM            *PROC
002413170706     D  PARMS            *PARMS
002414170706     D*----------------
002415170706     D* Richiamo a XATB
002416170706     D XATBDS        E DS
002417170706     D*-------------
002418170706     D* Parametri per ricerca codice ANATB00F
002419170706     D XTABDS        E DS
002420170706     D*----------------
002421170706     D* Parametri
002422170706     D*----------------
002423170707     D SDGHD4DS      E DS
002424170706     D*----------------
002425170706     D ANGG12DS      E DS
002426170706     D*----------------
002427170706     D $Efile          S              1
002428170706     D $RcdOK          S              1A
002429170706     D*----------------
002430170706     D UDateISO        S               D
002431170706     D DatMin          C                   Const('0001-01-01')
002432170706     D*----------------
002433170809     D SkSoc           S              3    Dim(500)
002434170809     D SkKey           S             15    Dim(500)
002435170809     D SkUni           S            256    Dim(500)
002436170706     D*-------------
003000000000     C*----------------------------------------------------*
003100000000     C*                MAIN LINE PROGRAM
003200000000     C*----------------------------------------------------*
003201170707     C*
003202170707     C                   Exsr      InzVar
003203170707     C*
003300000000     C                   EXSR      REPCAMPI
003400000000     C*
003500000000     C                   EXSR      ESEGUI
003600000000     C*
003700000000     C                   EXSR      ENDPGM
003800000000     C/EJECT
003900000000     C************************************************************
004000000000     C* Reperimento campi del file
004100000000     C************************************************************
004200000000     C     Repcampi      BEGSR
004300000000     C*
004400000000     C* Delete
004500000000     C                   CALL      'QCMDEXC'                            21
004600000000     C                   PARM                    CM1(1)
004700000000     C                   PARM      80            LEN              15 5
004800000000     C*
004900000000     C* OVRDBF
005000000000     C                   CALL      'QCMDEXC'                            21
005100000000     C                   PARM                    CM1(2)
005200000000     C                   PARM      80            LEN              15 5
005300000000     C*
005400000000     C* DSPFFD
005500000000     C                   CALL      'QCMDEXC'                            21
005600000000     C                   PARM                    CM1(3)
005700000000     C                   PARM      80            LEN              15 5
005800000000     C*
005801170706     C                   movel     *off          ISO3              1
005802170706     C                   movel     *off          ISO2              1
005803170706     C*
005900170706     C     *IN21         IFEQ      *OFF
006000000000     C*
006100000000     C                   OPEN      QADSPFFD
006200000000     C     1             SETLL     QADSPFFD                           21
006300000000     C*
006400000000     C     *IN21         DOWEQ     *OFF
006500000000     C*
006600000000     C                   READ      QADSPFFD                               21
006701170706     C                   IF        WHFLDI = 'CODISOG12'
006702170706     C                   Movel     *on           ISO3
006900170706     C                   ENDIF
006901170706     C                   IF        WHFLDI = 'CODISO2G12'
006902170706     C                   Movel     *on           ISO2
006903170706     C                   ENDIF
007000000000     C*
007001170706     C                   ENDDO
007002170706     C*
007100000000     C                   CLOSE     QADSPFFD
007200000000     C*
007300000000     C                   ENDIF
007400000000     C*
007500000000     C                   ENDSR
007600000000     C/EJECT
007700000000     C************************************************************
007800000000     C* Esegui aggiornamenti
007900000000     C************************************************************
008000000000     C     Esegui        BEGSR
008100000000     C*
008101170707     C*
008102170707     C                   Exsr      ElaboraG12
008103170707     C*
008104170707     C                   Exsr      WriteHD4
008105170707     C*
019500000000     C*
019600000000     C                   ENDSR
024100000000     C/EJECT
024200000000     C************************************************************
024300000000     C* OPERAZIONI INIZIALI
024400000000     C************************************************************
024500000000     C     *INZSR        BEGSR
024600000000     C*
024700000000     C                   ENDSR
024800000000     C/EJECT
024802170707     C************************************************************
024803170707     C* Lettura tabella G12
024804170707     C************************************************************
024805170707     C     ElaboraG12    BegSr
024806170707     C*
024807170707     C                   Z-add     0             K                 3 0
024808170707     C*
024809170707     C                   MOVE      'G12'         XTBCOD
024810170707     C*
024811170707     C                   MOVE      XscSoc        XTBAZI
024812170707     C                   MOVE      XscLin        XTBLIN
024813170707     C                   MOVE      '9'           XTBRIC
024814170707     C                   MOVE      '0'           XTBALC
024815170707     C                   MOVE      XscCut        XTBUTE
024816170707     C                   MOVE      '0'           XTBERR
024817170707     C                   CALL      'XATB'
024818170707     C                   PARM                    XATBDS
024819170707     C*
024820170707     C     XTBERR        IFNE      '0'
024821170707     C                   MOVEL     *ON           $EFILE
024822170707     C                   ENDIF
024823170707     C*
024824170707     C                   MOVEL     *OFF          $EFILE
024825170707     C                   MOVEL     *OFF          $RCDOK
024826170707     C*
024827170707     C     $EFILE        DOUEq     *ON
024828170707     C     $RCDOK        OrEq      *ON
024829170707     C*
024830170707     C                   MOVE      '3'           XTBRIC
024831170707     C                   MOVE      '0'           XTBERR
024832170707     C                   CALL      'XATB'
024833170707     C                   PARM                    XATBDS
024834170707     C*
024835170707     C     XTBERR        IFNE      '0'
024836170707     C                   MOVEL     *ON           $EFILE
024837170707     C                   ENDIF
024838170707     C*
024839170707     C     $EFILE        IfEq      *OFF
024840170707     C                   MoveL     XtbUni        AngG12DS
024843170707     C                   Exsr      SalvaDati
024846170707     C                   EndIf
024847170707     C*
024848170707     C                   EndDo
024849170707     C*
024850170707     C                   Endsr
024851170707     C************************************************************
024852170707     C* Salvataggio dati
024853170707     C************************************************************
024854170707     C     SalvaDati     BegSr
024855170707     C*
024856170707     C                   Add       1             K
024857170707     C                   MoveL     XtbKey        SkKey(K)
024858170707     C                   MoveL     XtbUni        SkUni(K)
024860170707     C*
024861170707     C                   Endsr
024862170707     C************************************************************
024863170707     C* Crea HD4
024864170707     C************************************************************
024865170707     C     WriteHD4      BegSr
024866170707     C*
024867170707     C                   Do        K             K1                3 0
024868170707     C*
024869170707     C                   MoveL     SkKey(k1)     XTBKEY
024870170707     C                   Move      'HD4'         XTBCOD
024871170707     C*
024872170707     C                   EXSR      RieHD4
024873170707     C* Scrivo se almeno uno dei due valori ISO è indicato
024874170707     C                   IF        codisoHD4 <> *blanks or
024875170707     C                             codiso2Hd4 <> *blanks
024876170707     C                   MOVE      XSCCUT        XTBUTE
024877170707     C                   MOVE      '5'           XTBRIC
024878170707     C                   CALLb     'XATB'
024879170707     C                   PARM                    XATBDS
024880170707     C*
024881170707     C                   ENDIF
024882170707     C*
024883170707     C                   EndDo
024884170707     C*
024885170707     C                   Endsr
024886170707     C************************************************************
024887170707     C* Riempimento HD4
024888170707     C************************************************************
024889170707     C     RieHD4        Begsr
024890170707     C*
024891170707     C                   Clear                   XTBUNI
024892170707     C                   Move      DatMin        XTBDSC
024893170707     C                   Move      '0'           XTBRIS
024894170707     C                   MoveL     SkUni(K1)     AngG12ds
024895170707     C                   MoveL     DESG12        DESHD4
024896170707     C                   MoveL     DESG12        DESCRHD4
024897170707     C                   If        Iso3=*on
024898170707     C                   Eval      codisoHd4 = %subst(SKUNI(K1):53:3)
024899170707     C                   Endif
024900170707     C                   If        Iso2=*on
024901170707     C                   Eval      codiso2Hd4 = %subst(SKUNI(K1):56:2)
024902170707     C                   Endif
024903170707     C                   MoveL     SDGHD4DS      XTBUNI
024904170707     C*
024905170707     C                   EndSr
024906170707     C************************************************************
024907170707     C* Inizializzazioni variabili
024908170707     C************************************************************
024909170707     C     InzVar        BegSr
024910170707      *
024911170707     C* Reperimento dati società
024912170707    >C                   MOVEL     'SOC001'      TIPXSC
024913170707     C                   MOVEL     *BLANK        SOCXSC
024914170707     C                   EXSR      REPSOC
024915170707     C     RTNXSC        IFNE      '1'
024916170707     C                   MOVEL     XSOCDS        SOC001
024917170707     C                   ELSE
024918170707     C                   RETURN
024919170707     C                   ENDIF
024920170707      *
024921170707     C     *DMY          Move      Udate         UDateISO
024922170707     C
024923170707     C                   EndSr                                                  *
024924170707     C************************************************************
024925170707     C* REPERIMENTO DATI SOCIETA'
024926170707     C************************************************************
024927170707     C     REPSOC        BEGSR
024928170707     C*
024929170707     C                   CALL      'SDGSOCDR'
024930170707     C                   PARM      'SOC001'      TIPXSC            6
024931170707     C                   PARM                    SOCXSC            3
024932170707     C                   PARM                    CDSXSC            9 0
024933170707     C                   PARM                    MODXSC            3
024934170707     C                   PARM      *BLANKS       RTNXSC            1
024935170707     C                   PARM                    XSOCDS
024936170707     C                   PARM                    KPJBA
024937170707     C*
024938170707     C                   If        RtnXsc     <> '1'
024939170707     C                   MoveL     XSOCDS        SOC001
024940170707     C                   Else
024941170707     C                   Exsr      EndPgm
024942170707     C                   EndIf
024943170707     C*
024944170707     C                   EndSr
024945000000     C************************************************************
025000000000     C* Fine programma
025100000000     C************************************************************
025200000000     C     ENDPGM        BEGSR
025300000000     C*
025400000000     C                   SETON                                        LR
025500000000     C                   Return
025600000000     C*
025700000000     C                   ENDSR
025800000000     C/EJECT
025900000000**
026000000000DLTF FILE(QTEMP/QADSPFFD)
026100170706DSPFFD FILE(ANGG12DS) OUTPUT(*OUTFILE) OUTFILE(QTEMP/QADSPFFD)
026200000000OVRDBF FILE(QADSPFFD) TOFILE(QTEMP/QADSPFFD)
