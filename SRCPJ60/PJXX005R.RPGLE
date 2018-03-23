000100170125     H DFTACTGRP(*NO) ACTGRP(*NEW)
000200170125     H BNDDIR('PJXBND':'YCO')
000300001013     H DECEDIT('0,') DATEDIT(*DMY.)
000400001009      *----------*                                                              *
000500001009      * PJXX005R *---------------------------------------------------*          *
000600001009      *----------*                                                   *          *
000700001009      *                                                              *          *
000800001013      *            REPERIMENTO DATI DICHIARAZIONI INTENTO            *          *
000900001009      *                                                              *          *
001000001009      *--------------------------------------------------------------*          *
001100001009      *------------*                                                            *
001200050114     FANDIC01L  IF   E           K DISK                                         *
001201050113G1422FANDIC04L  IF   E           K DISK                                         *
001202050114G1422F                                     RENAME(ANDIC000:ANDIC04)
001300001009      *--------------------------------------------------------------*
001400170125       /COPY GAITRASRC/SRCCONST,YDIC2017
001401170125       /COPY GAITRASRC/SRCPROTOPR,YDIC2017
001402170125       DCL-S intento_esito INT(10) IMPORT;
001403170125      *---------------
001500001009      * Passaggio Parametri
001600001009     D KPJBA         E DS
001700001009      *---------------
001800001009      *Dati di ambiente ottenuti da XSOC
001900001009     D SOC001        E DS                  EXTNAME(XSOC001DS)
002000001009      *-------------
002100001009      * Parametri ricevuti
002200001009     D PJXX005DS     E DS                  INZ
002203050131G1422D* Parametri per reperimento importi su ordini
002204050121G1422D PJGX020DS     E DS                  inz
002300001009      *-------------
002400001009      *
002500001009     D DATMIN          C                   CONST('0001-01-01')
002600001009     D DATMAX          C                   CONST('9999-12-31')
002700001009     D ZDateISO        S               D
002800001009     D ZDate           S              6S 0
002900001013     D WDateISO        S               D
002901050113G1422D TotFattu        S             19  3
002902050121G1422D Wrkdatai        S             10
002903050121G1422D Wrkdataf        S             10
003000001009      *
003100001009      *---------------------------------------------------------------*
003200001009      *           CORPO DEL PROGRAMMA
003300001009      *---------------------------------------------------------------*
003400001009      *
003500001009      *Inizializzazione variabili
003600001009     C                   EXSR      INZVAR
003601021022A0974C*
003602021022  "  C* Pgm chiamato con opzione =1 viene recuperata una Dic. d'intento
003603021022A0974C                   IF        OPZ005 = '1'
003700001009      *
003800050113G1422X*** Lettura di Andic01l
003801050113G1422 * - Lettura di Andic04l
003900001013      *                                                                         *
004000001013     C                   Move      CCF005        DicCF                          *
004100001013     C                   Move      KSC005        DicKSC                         *
004200050113G1422X***  K03DIC01      SETGT     ANDIC01L                                     *
004300050113  "  X***  K03DIC01      READPE    ANDIC01L                               21    *
004400050113  "  X***                                                                       *
004500050113  "  X***  *IN21         DOWEQ     '0'                                          * 2 DO
004501050113  "  C*                                                                         *
004502050114  "  C     K03Dic01      SetLL     AnDic04                                      *
004503050113  "  C                   Do        *Hival                                       *
004504050114  "  C     K03Dic01      Reade     AnDic04                                21    *
004505050113  "  C   21              Leave
004506050113  "  C*                                                                         *
004507050113  "  C* Reperimento anno della data interna                            ferim
004508050113  "  C                   extrct    dicDtInter:*Y AnnoInt           4 0
004509050113  "  C* la vista logica è descend, se anno dich.> anno rif. continua
004510050113  "  C* la vista logica è descend, se anno dich.< anno rif. esce
004511050113  "  C                   if        AnnoInt  > AnnoRif
004512050113  "  C                   iter
004513050113  "  C                   endif
004514050113  "  C                   if        AnnoInt  < AnnoRif
004515050113  "  C                   leave
004516050113  "  C                   endif
004517050113  "  C*
004518050113  "  C* Eseguo il controllo in base a com'è impostata la dich. intenti          *
004519050113  "  C                   Select
004520050113  "  C*
004521050113  "  C                   When      DICDtInDic <> *loval and
004522050113G1422C                             DICDtFiDic <> *loval
004600050113      *-                                                                        *
004700001013      *  Carico dati dich. intenti se data fatturazione è compresa              *
004800001013      *  nel range data inizio/data fine                                        *
004900021018E1438 *  e se la dich. int. non è revocata
005000001013      *-                                                                        *
005100021018E1438X***  DTF005        IfGE      DicDTINDIC
005200021018  "  X***  DTF005        AndLE     DicDTFIDIC
005300021018  "   *
005400021018  "  C                   if        DtF005 >= DICDtInDic and
005500021018  "  C                             DtF005 <= DICDtFiDic and
005600021018  "  C                             (DICDtRevoc = *loval or
005700021018E1438C                              DtF005 <= DICDtRevoc)
005800001013      *-                                                                        *
005900021018E1438 * Se si sta cercando una data revoca per un fornitore rendo
006000021018  "   * come numero e data i riferimenti interni
006100021018  "  C                   if        CCF005 = 'F'
006200021018  "  C                   z-add     DICNrInter    NDI005                         *
006300021018  "  C                   Move      DICDtInter    DtD005                         *
006400021018E1438C                   else
006500001013     C                   Z-add     DicNRCLI      NDI005                         *
006600001013     C                   Move      DicDTCLI      DTD005                         *
006700021018E1438C                   endif
006800021018E1438 *
006900001013     C                   Move      DicCDIVA      CIV005                         *
007000050113G1422X***                                                                       *
007100050113  "  X***                Seton                                          21      *
007200050113  "  X***                                                                       *
007300050113  "  X***                Else                                                   *
007400050113  "  X***                                                                       *
007500050113  "  X***  K03DIC01      READPE    ANDIC01L                               21    *
007501050113G1422C                   Leave
007600050113     C*                                                                         *
007700050113     C                   Endif                                                  * 3 END
007701050113G1422C*
007702050113  "  C                   When      DicAnno <> 0
007703050113  "  C*
007704050113  "  C* se l'anno è lo stesso della data riferimento
007705050113  "  C* e se la dich. int. non è revocata
007706050113  "  C                   if        DicAnno = AnnoRif and
007708050113  "  C                             (DICDtRevoc = *loval or
007709050113  "  C                              DtF005 <= DICDtRevoc)
007710050113  "  C*
007711050113  "  C* richiamo routine calcolo fatturato (una volta sola).
007712050113  "  C                   if        TotFattu = 0
007713050113  "  C                   exsr      CalcFattu
007714050113  "  C                   endif
007715050113  "  C*                                                                         *
007716050128  "  C* se totale inferiore al tetto stabilito dalla dich. d'intento            *
007717170407>>>>>C                   If        TotFattu <= DicImporto
007718050113  "  C*                                                                         *
007719050113  "  C                   if        CCF005 = 'F'
007720050113  "  C                   z-add     DICNrInter    NDI005                         *
007721050113  "  C                   Move      DICDtInter    DtD005                         *
007722050113  "  C                   else
007723050113  "  C                   Z-add     DicNRCLI      NDI005                         *
007724050113  "  C                   Move      DicDTCLI      DTD005                         *
007725050113  "  C                   endif
007726050113  "  C*
007727050113  "  C                   Move      DicCDIVA      CIV005                         *
007728050113  "  C                   Leave
007729050113  "  C*                                                                         *
007730050113  "  C                   Endif                                                  * 3 END
007731050113  "  C                   Endif                                                  * 3 END
007732050113  "  C*                                                                         *
007733050113G1422C                   EndSl                                                  * 3 END
007800001009      *                                                                         *
007900001009     C                   Enddo                                                  * 2 END
007901021022A0974C*                                                                         *
007902021022  "  C                   Endif                                                  * 2 END
007903021022  "  C*                                                                         *
007904021022  "  C* Pgm chiamato con opzione=2 viene controllata una Dic. d'intento         *
007905021022  "  C                   IF        OPZ005 = '2'                                 *
007906021022  "  C*                                                                         *
007907021022  "  C                   Move      CCF005        DicCF                          *
007908021022  "  C                   Move      KSC005        DicKSC                         *
007909021022  "  C                   Move      NDI005        DicNrCli                       *
007910021022  "  C                   Move      DTD005        DicDtCli                       *
007911021022  "  C     K05DIC01      CHAIN     ANDIC01L                           21        *
007912021022  "   *                                                                         *
007913050114A0974C     *IN21         IFEQ      '0'                                          * 2 DO
007914050114G1422C*
007915050114  "  C* Eseguo il controllo in base a com'è impostata la dich. intenti          *
007916050114  "  C                   Select
007917050114  "  C*
007918050114  "  C                   When      DICDtInDic <> *loval and
007919050114G1422C                             DICDtFiDic <> *loval
007920050114A0974 *-                                                                        *
007921021023  "   * Se data DTF005 è valorizzata e non è compresa nel range data            *
007922021023  "   * inizio/data fine o se la dich. int. è revocata                          *
007923021023  "   * valorizzo campo errore della ds
007924021023  "   *
007925021023  "  C                   if        DtF005 <> *loval and
007926021023  "  C                             (DtF005 < DICDtInDic or
007927021023  "  C                             DtF005 > DICDtFiDic or
007928021023  "  C                             (DICDtRevoc <> *loval and
007929021023  "  C                              DtF005 > DICDtRevoc))
007930021023  "   *-                                                                        *
007931021023  "  C                   Movel     '1'           ERR005
007932050114A0974C                   Endif                                                  * 2 END
007933050114G1422C*
007934050114  "  C                   When      DicAnno <> 0
007935050114  "  C*
007936050114  "  C* Se data riferimento valorizzata
007938050114  "  C                   If        DtF005 <> *loval
007939050114  "  C* Se anno riferimento è uguale a quello di competenza plafond
007940050114  "  C* e la dich. intenti non è revocata
007941050114  "  C                   If        DicAnno = AnnoRif and
007942050114  "  C                             (DICDtRevoc = *loval or
007943050114  "  C                              DtF005 <= DICDtRevoc)
007945050114  "  C* richiamo routine calcolo fatturato
007946050114  "  C                   exsr      CalcFattu
007950050114  "  C*                                                                         *
007951050114  "  C* se totale supera il tetto stabilito dalla dichiaraz. d'intento          *
007952050114  "  C* valorizzo campo errore della ds                                         *
007953050114  "  C                   If        TotFattu > DicImporto
007954050114  "  C                   Movel     '1'           ERR005
007955050114  "  C                   Endif                                                  * 2 END
007956050114  "  C*                                                                         *
007957050114  "  C                   Else                                                   * 2 END
007958050114  "  C* valorizzo campo errore della ds
007959050114  "  C                   Movel     '1'           ERR005
007960050114  "  C                   Endif                                                  * 2 END
007961050114  "  C                   Endif                                                  * 2 END
007962050712G1422C*
007963050714G1447C                   When      DicOpera = '1'
007964050712  "  C* Controllo che la dich. intenti non sia revocata
007965050712  "  C                   If        DICDtRevoc <> *loval and
007966050712  "  C                             DtF005 > DICDtRevoc
007967050712  "  C* valorizzo campo errore della ds
007968050712  "  C                   Movel     '1'           ERR005
007969050714  "  C                   Else                                                   * 2 END
007970050714  "  C* pongo Opz005='M' per indicare che è una dic. d'intenti manuale
007971050714  "  C                   Movel     'M'           MAN005
007972050714  "  C                   Endif                                                  * 2 END
007973050712G1447C*
007974050114G1422C                   EndSl
007975050114A0974 *-                                                                        *
007976021023  "  C                   Move      DicCDIVA      CIV005                         *
007977050114  "   *-                                                                        *
007978050114  "  C                   Else                                                   * 2 END
007979021023  "   * Non trovata dichiarazione valorizzo campo errore della ds
007980021022  "  C                   Movel     '1'           ERR005
007981021022  "   *-                                                                        *
007982021022  "  C                   Endif                                                  * 2 END
007983021022  "   *-                                                                        *
007984021022A0974C                   Endif                                                  * 2 END
008011001009      *
008100001009     C                   Exsr      EndPgm
008200001009      *
008300001009      *---------------------------------------------------------------*
008400001009      * Fine Programma
008500001009      *---------------------------------------------------------------*
008600001009     C     EndPgm        BEGSR
008700001009      *
008800001009      * Non trovata dichiarazione valorizzo campo errore della ds
008900001013     C                   If        NDI005 = 0 and DTD005 = WDateISO and
009000001013     C                             CIV005 = ' '
009100001009     C                   Movel     '1'           ERR005
009200001009     C                   Endif
009300001009      * passaggio dati al pgm chiamante
009400001013     C                   Movel     PJXX005DS     KPJBU
009500001009      *
009501170125       Intento_Finalize();
009502170125      *
009600001009     C                   SetOn                                            LR
009700001009      *
009800001009     C                   ENDSR
009801050114G1422C************************************************************
009802050114  "  C* Calcolo fatturato per un dato anno di competenza
009803050114  "  C************************************************************
009804050114G1422C     CalcFattu     BEGSR
009805050113     C*
009806050121     C                   Z-Add     *Zero         TotFattu
009807050121     C* Imposto le date inizio e fine periodo
009808050121     C                   move      DicAnno       AnnoAlfa          4
009809050121     C                   eval      wrkdatai = AnnoAlfa+'-01-01'
009810050121     C                   eval      wrkdataf = AnnoAlfa+'-12-31'
009839050128     C*
009841050131     C* Chiamo una utility che reperisce l'importo totale degli ordini
009842050131     C* in stato spedito + fatturato contabilizzato e non.
009843050121     C*
009845050121     C                   RESET                   PJGX020DS
009846050121     C                   eval      Soc020 = Soc005
009847050121     C                   eval      unt020 = unt005
009848050121     C                   eval      ccf020 = ksc005
009849050121     C                   move      wrkdatai      DatIn020
009850050121     C                   move      wrkdataf      DatFin020
009851050121     C                   eval      Opz020 = '03'
009852050131     C                   eval      FlgCnb020 = xscon
009853050211     C                   Z-add     DicNRCLI      NrDic020
009854050121     C                   Movel     PJGX020DS     Kpjbu
009855050121     C*
009856050121     C                   call      'PJGX020R'
009857050121     C                   parm                    Kpjba
009858050121     C*
009859050121     C                   Movel     Kpjbu         PJGX020DS
009860050121     C*
009861050121     C                   If        err020 = *off
009862050121     C                   Add       TotImp020     TotFattu
009863050121     C                   EndIf
009864050121     C*
009865170712       Intento_NewFattura( dicSocieta : dicKsc : dtf005 : *ZERO );
009867170712       IF intento_esito = INTENTO_ESITO_OK
009870170712       OR intento_esito = INTENTO_ESITO_NON_TROVATO;
009871170125         totFattu = Intento_GetImponibileFatturatoSenzaIvaAnno();
009872170125         dicImporto = Intento_GetImportoDichiarazioniAnno();
009873170125       ENDIF;
009874170125       Intento_EndFattura();
009875170125     C*
009876050114G1422C                   endsr
009900001009      *---------------------------------------------------------------*
010000001009     C     INZVAR        BEGSR
010100001009      *
010200001013     C                   Movel     KPJBU         PJXX005DS
010300001009      *
010400001009      *Dati società
010500001013     C                   Movel     'SOC001'      TIPXSC
010600001013     C                   Movel     Soc005        SOCXSC
010700001009     C                   EXSR      REPSOC
010800001013      * Errore reperimento dati società
010900001013     C                   If        RtnXSc     =  *On
011000001009     C                   Exsr      EndPgm
011100001009     C                   EndIf
011200001009      *
011300001009      *Inizializzo variabili di appoggio
011400001013      *-                                                                        *
011401021023A0974C                   IF        OPZ005 = '1'
011500001013     C                   Z-add     0             NDI005                         *
011600001013     C                   Move      Datmin        DTD005                         *
011601021023A0974C                   ENDIF
011700001013     C                   Move      *blanks       CIV005                         *
011800001013     C                   Move      Datmin        WDateISO                       *
011900001013     C                   Move      '0'           ERR005                         *
011901050714G1447C                   Move      *Blank        MAN005                         *
011902050113G1422C* azzero totale fatturato
011903050113  "  C                   clear                   TotFattu
011904050113  "  C* reperimento anno della data di riferimento passata
011905050113G1422C                   extrct    Dtf005:*Y     AnnoRif           4 0
012000001013      *-                                                                        *
012100001009     C                   ENDSR
012200001009      *---------------------------------------------------------------*
012300001009      * REPERIMENTO DATI SOCIETA'
012400001009      *---------------------------------------------------------------*
012500001009     C     REPSOC        BEGSR
012600001009      *
012700001009     C                   CALLB     'XSOC'
012800001009     C                   PARM                    TIPXSC            6
012900001009     C                   PARM                    SOCXSC            3
013000001009     C                   PARM                    CDSXSC            9 0
013100001009     C                   PARM                    MODXSC            3
013200001009     C                   PARM      *BLANKS       RTNXSC            1
013300001013     C                   PARM                    SOC001
013400001009     C                   PARM                    KPJBA
013500001009      *
013600001009     C                   ENDSR
013700001009      *---------------------------------------------------------------*
013800001009      * OPERAZIONI INIZIALI
013900001009      *---------------------------------------------------------------*
014000001009     C     *InzSr        BEGSR
014100001009      *
014200001009     C     *DMY          Move      Udate         ZDateISO
014300001009     C     *JobRun       Move      ZDateISO      ZDate
014400001009      *
014500001009     C     *Entry        PList
014600001009     C                   Parm                    Kpjba
014700001009      *
014701170125       Intento_Init(kpjba);
014702170125      *
014800001009     C                   ENDSR
014900001009      *---------------------------------------------------------------*
015000001009      * DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
015100001009      *---------------------------------------------------------------*
015200001009     C     DefCam        BEGSR
015300001009      *
015400001009      * ANDIC01L
015500001009     C     K03DIC01      KLIST
015600001009     C                   KFLD                    Xscsoc
015700001009     C                   KFLD                    DicCF
015800001009     C                   KFLD                    DicKSC
015801021022A0974 *
015802021022  "  C     K05DIC01      KLIST
015803021022  "  C                   KFLD                    Xscsoc
015804021022  "  C                   KFLD                    DicCF
015805021022  "  C                   KFLD                    DicKSC
015806021022  "  C                   KFLD                    DicNrCli
015807021022A0974C                   KFLD                    DicDtCli
015900001009      *
016000001009      * Generica solo società
016100001009     C     KXscsoc       KLIST
016200001009     C                   KFLD                    Xscsoc
016300001009      *
016400001009     C                   ENDSR
016500001009      *---------------------------------------------------------------*
