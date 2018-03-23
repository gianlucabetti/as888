000100040112     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200040113     H*PARMS BNDDIR(PJXBND) cvtopt(*none)
000300040112     H DECEDIT('0,') DATEDIT(*DMY.)
000400040112      *--------------------------------------------------------------*          *
000500040112      *            Reperimento dichiarazioni di intento              *          *
000600040113      *--------------------------------------------------------------*          *
000700040114      * ErroreX11=*on: significa che c'è almeno una dichiarazione di
000800040114      *                intento in contrasto con il codice esenzione o
000900040114      *                che non ce ne è neanche una valida.
001000040112      *------------*                                                            *
001100040115     FANDIC04L  IF   E           K DISK                                         *
001200040115     FANRIV01L  IF   E           K DISK
001300150122R160 FSDGDIF01L IF   E           K DISK
001400040112      *--------------------------------------------------------------*
001500040112      * Parametri ricevuti
001600040113     D PJXX011DS     E DS
001700040112      *-------------
001800040112     D* Ds multipla dichiarazioni di intento reperite
001900040113     D SKDich          DS                  occurs(30)
002000040113     D SKAndic                             like(Andic)
002100040114     D ImportF                       19  3
002200040113      *-------------
002300040113     D*ds multipla per parametri in uscita
002400040113     D SKDich011       DS                  occurs(30)
002500040113     D SKAnDic11                           like(Andic)
002600040114     D ImportFX11                    19  3
002700040113      *-------------
002800040113     D* Tracciato ds
002900040113     D AnDic         E DS                  EXTNAME(ANDIC00F)
003000040113      *-------------
003100040112     D* Tabella dichiarazioni di intento
003200040113     D angg48ds      E DS
003300040113      *-------------
003400040113     D* Richiamo a XATB
003500040113     D XATBDS        E DS
003600040114     D*-------------
003700040114     D* Parametri per reperimento fatturato
003800040114     D KPJBA         E DS
003900040114     D PJXX026DS     E DS                  inz
004000040113     D*-------------
004100040114     D* Campi di lavoro
004200040114     D wrkdata         S             10
004300040114     D TotFattu        S             19  3
004400040114     D TotImpDic       S             19  3
004500040122     D WrkRifIva       S                   like(RifIvaX11)
004600150123R160 D WrkDtRicP       S                   like(DICDTINDIC)
004700150123R160 D FlDtRicP        S              1A
004800040114     D*-------------
004900040113     D* Reperimento nome PGM
005000040113     D STATUS         SDS           333
005100040113     D  DSPGM            *PROC
005200040113     D  PARMS            *PARMS
005300040112      *---------------------------------------------------------------*
005400040112     C*                MAIN LINE PROGRAM
005500040112      *---------------------------------------------------------------*
005600040112      *
005700040112     C* Operazioni iniziali
005800040112     C                   EXSR      Inzvar
005900040112      * Esecuzione programma
006000040112     C                   EXSR      Esegui
006100040112      * Fine programma
006200040112     C                   EXSR      Endpgm
006300040112     C************************************************************
006400040112     C* Fine Programma
006500040112     C************************************************************
006600040112     C     ENDPGM        BEGSR
006700040113     C*
006800040113     c* se richiesta la ds multipla
006900040113     c                   if        parms > 1
007000040113     c* passaggio
007100040113     c                   do        30            $X                3 0
007200040113     c     $x            occur     skDich
007300040113     c     $x            occur     skDich011
007400040114     C                   move      SkAndic       SkAndic11
007500040114     C                   move      ImportF       ImportFx11
007600040113     C                   enddo
007700040113     c
007800040113     c                   endif
007900040112     C*
008000040112     C                   SetOn                                            RT
008100040112     C                   Return
008200040112     C*
008300040112     C                   ENDSR
008400040112     C************************************************************
008500040112     C* Esecuzione programma
008600040112     C************************************************************
008700040112     C     Esegui        BEGSR
008800040112     C*
008900040112     C* Legge dichiarazioni di intento e carica ds multipla
009000040112     C*
009100040112     c                   exsr      LeggiDic
009200040112     c*
009300040113     C* Emissione a video delle dichiarazioni se richiesto e se presente err
009400040113     c                   if        EmettiX11 = *on and ErroreX11=*on
009500040112     c                   call      'PJXX011R1'
009600040112     c                   parm                    PJXX011DS
009700040113     c                   parm                    SkDich
009800040113     C                   PARM                    RETX11            1
009900040112     c                   endif
010000040112     C*
010100040112     C                   ENDSR
010200040112     C************************************************************
010300040112     C* Lettura delle dichiarazioni di intento
010400040112     C************************************************************
010500040112     C     LeggiDic      BEGSR
010600040113     C*
010700040113     c                   z-add     0             Ind               3 0
010800040112     C*
010900040115     c     k03dic01      Setll     Andic04l
011000040112     c                   do        *hival
011100040115     c     k03dic01      reade     Andic04l                               21
011200040112     c   21              leave
011300040112     c*
011400040113     c* reperimento anno della data interna, se diverso da anno data riferim
011500040113     c* ento, la dichiarazione non va considerata
011600040112     c                   extrct    dicDtInter:*Y AnnoInt           4 0
011700041222B0764x****               if        AnnoRif <> AnnoInt
011800041222  "  x****               leave
011900041222  "  x****               endif
012000041222  "  c* la vista logica è descend, se anno dich.> anno rif. continua
012100041222  "  c* la vista logica è descend, se anno dich.< anno rif. esce
012200041222  "  c                   if        AnnoInt  > AnnoRif
012300041222  "  c                   iter
012400041222  "  c                   endif
012500041222  "  c                   if        AnnoInt  < AnnoRif
012600041222  "  c                   leave
012700041222B0764c                   endif
012800040113     c*
012900040112     c* reperimento codice esenzione o aliquota da tabella G48
013000040112     c* per verificare se la dichiarazione si riferisce alla fattura
013100040112     c* in esame
013200040112     c                   exsr      LeggiG48
013300040113     c* se non trova codice riferimento esenzione con questo codice dichiara
013400040113     c* zione, continua a provare con tutte le dichiarazioni
013500040112     c                   if        TrovatoG48 <> *on
013600040113     c                   iter
013700040112     c                   endif
013800040123     c*
013900040123     c* impostato ad *off per tutti i casi che non rientrano nell'errore
014000040115     c                   eval      DicAnnulla=*off
014100040112     C*
014200040112     C* Dichiarazione valida: controlli
014300040113     c* CASO 1:sola operazione sempre ok
014400040122b1   c                   if        DicOpera = *on
014500040122     c                   exsr      Caso1
014600040122e1   c                   endif
014700040113     c*
014800040113     C* CASO 2:per periodo
014900040122b1   c                   if        DicDtInDic <> *loval and dicDtFiDic
015000040113     c                             <> *loval
015100040122     c                   exsr      Caso2
015200040122e1   c                   endif
015300040113     c*
015400040113     C* CASO 3:per importo
015500040122     c* se presente anno con importo
015600040122 b1  c                   if        DicAnno <> 0
015700040122     c                   exsr      Caso3
015800040122 e1  c                   endif
015900040112     c*
016000040112     c                   enddo
016100040113     c*
016200040113     c* se non ha trovato nessun codice esenzione nella tabella G48, errore
016300040122     c                   if        Valido <> *on and rifivax11 <> *blanks
016400040114     c                   eval      erroreX11=*on
016500040113     c                   endif
016600040112     C*
016700040112     C                   ENDSR
016800040122     C************************************************************
016900040122     C* CASO1 - operazione specifica
017000040122     C************************************************************
017100040122     C     Caso1         begsr
017200040123     c*
017300040122     c* stesso test sia per riferimento iva che aliquota
017400040122     C*
017500040123     c* controlli per data revoca solo in caso di riferimento iva, perchè
017600040123     c* in caso di aliquota sia che ci sia o che non ci sia la dichiarazione
017700040123     c* non interessa
017800040123     c                   if        RifIvax11 <> *blanks
017900040123     c                   exsr      CtrDtRevoc
018000040123     c                   endif
018100040122     c* carico ds multipla
018200040122     c                   exsr      CaricaDs
018300040122     C*
018400040122     c                   endsr
018500040122     C************************************************************
018600040122     C* CASO2 - periodo
018700040122     C************************************************************
018800040122     C     Caso2         begsr
018900040122     C*
019000150122R160 C                   If        CFX11         = 'C'
019100150122R160 C                   Exsr      RepDtRic
019200150123R160 C                   If        FlDtRicP      = *On
019300150123R160 c                   Move      WrKDtRicP     dicDtInDic
019400150122R160 c                   EndIf
019500150122R160 C                   EndIf
019600150122R160 c*
019700040123     c* Se data riferimento compresa nel periodo indicato
019800040123B1   c                   if        DtRifX11 >=dicDtInDic and DtRifX11<=
019900040122     c                             DicDtFiDic
020000040122     c*
020100040122     c* è ok nel caso in cui arrivi il riferimento iva
020200040122b2   c                   if        rifIvaX11 <> *blanks
020300040123     c* controllo solo data revoca
020400040123     c                   exsr      CtrDtRevoc
020500040122     c                   else
020600040123     c* se arriva l'aliquota NON è ok se non è stata revocata o se data revo
020700040123     c* ca superiore
020800040123b3   c                   if        DicDtRevoc =  *loval or  DicDtRevoc >
020900040123     c                             DtRifX11
021000040122     c                   eval      DicAnnulla='6'
021100040122     c                   eval      erroreX11=*on
021200040123e3   c                   endif
021300040123     c*
021400040122e2   c                   endif
021500040122     c*
021600040123X1   c                   else
021700040122     c*
021800040122     c* se data riferimento NON compresa nel periodo
021900040123     c* se riferimento iva NON è ok se non c'è data revoca o se c'è ma
022000040123     c* superiore alla data di riferimento
022100040123b2   c                   if        RifIvaX11 <> *blanks
022200040123b3   c                   if        DicDtRevoc =  *loval or  DicDtRevoc >
022300040123     c                             DtRifX11
022400040122     c                   eval      erroreX11=*on
022500040122     c                   eval      DicAnnulla='1'
022600040123e3   c                   endif
022700040123     c* controllo solo data revoca
022800040123     c                   exsr      CtrDtRevoc
022900040122     c                   else
023000040122     c* è ok in caso di aliquota
023100040123     c* e non devo controllare data revoca
023200040122e2   c                   endif
023300040122     c*
023400040123E1   c                   endif
023500040122     c*
023600040122     c* carico ds multipla
023700040122     c                   exsr      CaricaDs
023800040122     C*
023900040122     c                   endsr
024000040122     C************************************************************
024100040122     C* CASO3 - anno/importo
024200040122     C************************************************************
024300040122     C     Caso3         begsr
024400040122     c*
024500040122     c* da testare solo se arriva il codice esenzione
024600040122     c*
024700040122 b1  c                   if        rifIvax11 <> *blanks
024800040122     c*
024900040122     c* se l'anno non è dello stesso anno
025000040122     c* della data riferimento, evidenzio in rosso
025100040122 b2  c                   if        DicAnno <> AnnoRif
025200040122     c                   eval      erroreX11=*on
025300040122     c                   eval      DicAnnulla='3'
025400040122     c                   else
025500040122     c* se ok richiamo routine calcolo fatturato per verificare importo
025600040122     c* realizzato fino ad ora (lo richiamo una volta sola)
025700040122 b3  c                   if        TotFattu = 0
025800040122     c                   exsr      CalcFattu
025900040122 e3  c                   endif
026000040122     c* imposto nella ds il totale fatturato calcolato dal driver per
026100040122     c* emetterlo a video
026200040122     c                   z-add     Imponb026     TotFattu
026300040204     c* aggiungo importo riga in esame (NO, PERCHÈ C'È GIA' NEL TOTALE)
026400040204     c*******            add       ImportoX11    TotFattu
026500040122     c* il limite importo stabilito in andic va sommato (se ce ne è + di 1)
026600040122     c* se non revocata
026700040122 b3  c                   if        DicDtREVOC  = *loval or
026800040122     c                             DicDtREVOC >  DtRifX11
026900040122     c                   add       DicImporto    TotImpDic
027000040122 e3  c                   endif
027100040122     c* se importo riga esenzione sommato al totale calcolato supera il
027200040122     c* tetto stabilito dalla dichiarazione di intento, errore in rosso
027300040122 b3  c                   if        TotFattu   > TotImpDic
027400040122     c                   eval      DicAnnulla='4'
027500040122     c                   eval      ErroreX11=*on
027600040122 e2  c                   endif
027700040122 e2  c                   endif
027800040123     c*
027900040123     c* controllo data revoca
028000040123     c                   exsr      ctrDtRevoc
028100040122     c*
028200040122     C                   else
028300040122     c* se arriva aliquota è ok
028400040122 e1  c                   endif
028500040122     c*
028600040122     c* carico ds multipla sempre
028700040122     c                   exsr      CaricaDs
028800040122     C*
028900040122     c                   endsr
029000040113     C************************************************************
029100040113     C* Carica Ds multipla
029200040113     C************************************************************
029300040113     C     CaricaDs      begsr
029400040113     c*
029500040113     c                   add       1             Ind
029600040113     c                   if        Ind <=30
029700040113     c     Ind           occur     sKDich
029800040113     c                   move      Andic         SkAndic
029900040114     c                   z-add     TotFattu      ImportF
030000040113     c                   endif
030100040123     c*
030200040123     c* imposto campo VALIDO = *ON che serva a dire che c'è ALMENO UNA
030300040123     c* dichiarazione valida
030400040123b3   c                   if        DicAnnulla =*off
030500040123     c                   move      *on           Valido            1
030600040123e3   c                   endif
030700040113     C*
030800040113     C                   ENDSR
030900040122     C************************************************************
031000040123     C* Controlli per data revoca solo in caso di riferimento iva
031100040122     C************************************************************
031200040122     C     CtrDtRevoc    begsr
031300040122     c*
031400040122     c* se presente data revoca e questa è inferiore alla data riferimento
031500040122     c* la dichiarazione è come se fosse annullata e va evidenziata in
031600040122     c* rosso
031700040122b2   c                   if        DicDtREVOC <> *loval and
031800040122     c                             DicDtREVOC <= DtRifX11
031900040122     c                   eval      DicAnnulla='2'
032000040122     c                   eval      ErroreX11 =*on
032100040123     c                   endif
032200040122     c*
032300040122     C                   ENDSR
032400040112     C************************************************************
032500040112     C* Legge tabella G48
032600040112     C************************************************************
032700040112     C     LeggiG48      begsr
032800040112     c*
032900040112     C                   Clear                   CodiceG48         3
033000040112     C                   Clear                   TrovatoG48        1
033100040112     c*
033200040112     C                   MOVE      '9'           XTBRIC
033300040113     C                   MOVE      SocietaX11    XTBAZI
033400040112     C                   MOVE      'G48'         XTBCOD
033500040112     C                   MOVE      *BLANKS       XTBKEY
033600040112     C                   CALLb     'XATB'
033700040112     C                   PARM                    XATBDS
033800040112     C*
033900040112     C                   MOVEL     *OFF          $EFILE            1
034000040112     C                   MOVEL     *OFF          $RCDOK            1
034100040112     C*
034200040112     C     $EFILE        DOUEQ     *ON
034300040112     C     $RCDOK        OREQ      *ON
034400040112     C*
034500040112     C                   MOVE      '3'           XTBRIC
034600040113     C                   MOVE      SocietaX11    XTBAZI
034700040112     C                   MOVE      'G48'         XTBCOD
034800040112     C                   CALLb     'XATB'
034900040112     C                   PARM                    XATBDS
035000040122     c* errore
035100040122b1   C                   IF        XtbErr <> *off
035200040112     c                   eval      $Efile= *on
035300040112     c                   else
035400040122     c* no errore
035500040113     c                   eval      codiceG48= %subst(xtbkey:3:3)
035600040122     c* se codice tabella = codice dichiarazione  d'intento
035700040122b2   c                   if        codiceG48=DicCdIva
035800040112     c                   eval      AngG48ds = xtbuni
035900040112     c* se entrambi diversi dalla fattura in caricamento, continuo a leggere
036000040122b3   c                   if        KscG48<>RifIvaX11 and RifIvax11 <> *blank
036100040112     c                   iter
036200040112     c                   else
036300040122     c* se uguali, oppure se ricevuta aliquota
036400040122     c* se ricevuta aliquota, controllo plafond del codice di esenzione
036500040122     c* trovato in tabella G48
036600040122b4   c                   if        aliqX11 <> 0
036700040122     c                   eval      WrkrifIva = kscg48
036800040122     c                   exsr      CtrPlafond
036900040122     c* se non plafond, ne leggo un altro
037000040122b5   c                   if        NoPlafond = *on
037100040122     c                   iter
037200040122x5   c                   else
037300040122     c                   eval      TrovatoG48=*on
037400040122     c                   leave
037500040122e5   c                   endif
037600040122     c                   else
037700040122     c* no aliquota, si riferimento
037800040122     c                   eval      TrovatoG48=*on
037900040122     c                   leave
038000040122e4   c                   endif
038100040122     c*
038200040122e3   c                   endif
038300040122e2   c                   endif
038400040112     C*
038500040122e1   c                   ENDIF
038600040122e0   C                   ENDDO
038700040112     C*
038800040112     c                   ENDSR
038900040114     c*
039000040114     C************************************************************
039100040114     C* Calcolo fatturato per un dato codice di esenzione IVA
039200040114     C************************************************************
039300040114     C     CalcFattu     BEGSR
039400040114     C*
039500040114     C* decodificaconto
039600040114     C                   CALLB     'XCAPCLIFOR'
039700040114     C                   PARM                    societaX11
039800040416C1754X***                PARM      'F'           kcc               6
039900040416C1754C                   PARM      CFX11         kcc               6
040000040114     C                   PARM                    kscx11
040100040114     C*
040200040114     C                   RESET                   PJXX026DS
040300040114     C                   eval      societa026 = SocietaX11
040400040114     C                   eval      unita026   = unitax11
040500040114     C                   eval      ctb026     = 'CG'
040600040114     C                   eval      kcc026     = kcc
040700040114     C                   eval      ksc026     = kscx11
040800040114     C                   eval      rifiva026  = rifivax11
040900040114     C                   eval      flgdef026  = *on
041000040114     c* no i provvisori
041100040114     C                   eval      flgpro026  = *off
041200040114     c* data competenza plafond
041300040114     C                   eval      TipDat026='P'
041400040114     c                   move      DicAnno       AnnoAlfa          4
041500040114    >C                   eval      wrkdata    = AnnoAlfa+'-'+'01'+'-'+
041600040114     c                                          '01'
041700040114     c                   move      wrkdata       dti026
041800040114    >C                   eval      wrkdata    = AnnoAlfa+'-'+'12'+'-'+
041900040114     c                                          '31'
042000040114     c                   move      wrkdata       dtf026
042100150122R160 c*
042200150122R160 C                   If        CFX11         = 'C'
042300150122R160 C                   Exsr      RepDtRic
042400150123R160 C                   If        FlDtRicP      = *On
042500150123R160 c                   Move      WrKDtRicP     dti026
042600150122R160 c                   EndIf
042700150122R160 C                   EndIf
042800150122R160 c*
042900040114     C                   CALL      'PJXX026R'
043000040114     C                   PARM                    KPJBA
043100040114     C                   PARM                    PJXX026DS
043200040114     c*
043300040114     c                   endsr
043400150122R160 C************************************************************
043500150122     C* Recupera data ricevuta trasmissione
043600150122R160 C************************************************************
043700150122     C     RepDtRic      BEGSR
043800040112     C*
043900150123     C                   Eval      FlDtRicp      = *Off
044000150122     C*
044100150122     C                   Eval      DIFSocieta= SocietaX11
044200150122     C                   Eval      DIFCF     = CFX11
044300150122     C                   Eval      DIFDTINTER= DICDTINTER
044400150122     C                   Eval      DIFNRINTER= DICNRINTER
044500150122     C     K04DIF01      Chain     SDGDIF01L
044600150122     C                   If        %found(SDGDIF01l)  and
044700150123     c                             DIFDtRicP  <> *Loval
044800150123     c                   Eval      WrKDtRicP   =  DIFDtRicP
044900150123     C                   Eval      FlDtRicP      = *On
045000150122     c                   Endif
045100150122     C*
045200150122R160 c                   endsr
045300150122     C************************************************************
045400150122     C* OPERAZIONI INIZIALI
045500150122     C************************************************************
045600150122     C     *INZSR        BEGSR
045700150122     C*
045800040112     C* Reperimento parametri
045900040112     C*
046000040112     C     *ENTRY        PLIST
046100040114     C                   PARM                    PJXX011Ds
046200040114     c* opzionale (ds multipla con dichiarazioni trovate)
046300040113     C                   PARM                    SkDich011
046400040112     C*
046500040112     C                   ENDSR
046600040112      /EJECT
046700040112     C************************************************************
046800040112     C* INIZIALIZZAZIONE VARIABILI
046900040112     C************************************************************
047000040112     C     INZVAR        BEGSR
047100040113     c*
047200040113     c                   clear                   Ind
047300040112     C*
047400040112     c* pulizia ds multipla
047500040113     c                   do        30            $X                3 0
047600040113     c     $x            occur     skDich
047700040113     C                   clear                   skAndic
047800040112     C                   enddo
047900040114     c* azzero errore
048000040114     c                   eval      ErroreX11=*off
048100040114     c                   eval      Valido = *off
048200040114     c* azzero totale fatturato
048300040114     c                   clear                   TotFattu
048400040114     c                   clear                   TotImpDic
048500040122     c*
048600040122     c* controllo plafond
048700040203     c                   clear                   WrkRifIva
048800040122     c                   if        RifIvax11 <> *blanks
048900040122     c                   eval      WrkRifIva = RifIvaX11
049000040122     c                   exsr      CtrPlafond
049100040122     c                   if        NoPlafond = *on
049200040122     c                   exsr      endpgm
049300040122     c                   endif
049400040122     c                   endif
049500040112     C*
049600040112     c* reperimento anno della data di riferimento passata
049700040113     c                   extrct    DtRifX11:*Y   AnnoRif           4 0
049800040112     C*
049900040112     C                   ENDSR
050000040122      /EJECT
050100040122     C************************************************************
050200040122     C* controllo plafond
050300040122     C************************************************************
050400040122     C     CtrPlafond    BEGSR
050500040122     C*
050600040122     c                   clear                   NoPlafond         1
050700040122     C*
050800040122     c* Se il codice esenzione IVA ha tipo plafond = *on
050900040122     C                   eval      RivSocieta= SocietaX11
051000040122     C                   eval      RivTpReg=TpRegX11
051100040122     C                   eval      RivAliq = 0
051200040122     C                   eval      RivRifIva = WrkRifIva
051300040122     C     K04Riv01      chain     AnRiv01l
051400040122     C                   if        not %found(anRiv01l) or
051500040122     C                             %found(anRiv01l)
051600040122     c                             and RivPl1<>*on
051700040416C1754C* flag esportazioni (per vendite o corrispettivi)
051800040416c1754c                             and Rivpl2<>*on
051900040122     c                   eval      NoPlafond = *on
052000040122     c                   endif
052100040122     C*
052200040122     c                   endsr
052300040112      /EJECT
052400040112     C************************************************************
052500040112     C* DEFINIZIONE KLIST
052600040112     C************************************************************
052700040112     C     DefKlist      BEGSR
052800040112     C*
052900040112     C* klist
053000040112     C     K03Dic01      KLIST
053100040113     C                   KFLD                    SocietaX11
053200040113     C                   KFLD                    CFX11
053300040113     C                   KFLD                    KscX11
053400040115      * ANRiv01L
053500040115     C     K04Riv01      klist
053600040115     C                   kfld                    RivSocieta
053700040115     C                   kfld                    RivTpReg
053800040115     C                   kfld                    RivAliq
053900040115     C                   kfld                    RivRifIva
054000150122R160 C*
054100150122 "   C* klist
054200150122 "   C     K04DIF01      KLIST
054300150122 "   C                   KFLD                    DIFSocieta
054400150122 "   C                   KFLD                    DIFCF
054500150122 "   C                   KFLD                    DIFDtInter
054600150122R160 C                   KFLD                    DIFNrInter
054700150122     C*
054800040112     C                   ENDSR
054900040112      /EJECT
