000100981118      *-------------------------------------------------------------------------*
000200060621      * Stampa Autodichiarazione da inviare al cliente                          *
000300981118      *-------------------------------------------------------------------------*
000400060621      * La stampa è possibile se è stata eseguita la fase 400 e la CA non risul-*
000500060621      * ta chiusa con causali diverse da quelle di pagamento                    *
000600981118      * Questo pgm non avanza di fase la C.A.                                   *
000700981118      *-------------------------------------------------------------------------*
000800981118
000900981118      ****************************************************************
001000981118      *  FASI C.A. FISSATE A PROGRAMMA
001100981118      ****************************************************************
001200060621      * 400 = Preavviso di danno                            WFPAD
001300060621      * 900 = Chiusura C.A.                                 WFCHI
001400981118      ***************************************************************
001500981118
001600981118      ****************************************************************
001700981118      *  RIEPILOGO INDICATORI
001800981118      ***************************************************************
001900060621      * 05 - SPROTEGGO P.O. IN GESTIONE
002000060621      * 06 - NON ABILITO TASTO FUNZIONALE F18
002100981118      * 28 - ERRORE GENERICO DSPF
002200981118      * 30 - COMODO
002300981118      * 31 - COMODO
002400981118      * 40 - ERRORE C.A. per stampa
002500981118      * 41 - ERRORE data stampa per ristampa
002600981118      ***************************************************************
002700981118
002800981118     H DECEDIT('0,') DATEDIT(*DMY.)
002900981119      *------------------------------------------------------------------------*
003000060621     fFIDN36D   CF   E             WORKSTN
003100060627     fFNDCT01L  IF   E           K DISK
003200060621     fAZORG01L  IF   E           K DISK
003300981118      *------------------------------------------------------------------------*
003400981118      *
003500981118      *   C A M P I
003600060622     d Kaac            S                   LIKE(DCTaac)
003700060622     d Kfil            S                   LIKE(DCTfil)
003800060622     d Knca            S                   LIKE(DCTnca)
003900060622     d Wv1cdts         S                   LIKE(V1Cdts)
004000981118
004100060622     d xx              S              3  0
004200060622     d W0140           S             14  0
004300060622     d Wdtgio          S              8  0
004400060622     d dateu           S              8  0
004500060622     d WPTF            S              4
004600060622     d WFPTF           S              7
004700060622     d WAfca           S              3
004800060622     d W001A           S              1
004900060622     d Wdataeur        S             10
005000060622     d Wfpad           S                   like(DCTfca) inz(400)
005100060622     d Wfchi           S                   like(DCTfca) inz(900)
005200060622     d DATA_eur        S               D   DATFMT(*eur)
005300981118      *
005400981118      *   S C H I E R E
005500060630     d MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
005600060622     d L6              S              3  0 DIM(30)                              P.O. GESTITI
005700981118      *
005800981118      *   D S   I N T E R N E / E S T E R N E
005900060622     d WLBDAT          DS                  INZ
006000060622     d  G02DAT                 1      8  0
006100060622     d  G02INV                 9     16  0
006200060622     d  G02ERR                17     17
006300060622     d  G02TGI                18     22  0
006400060627
006500060627     d Param           ds
006600060627     d  poges                  1      3  0
006700060627     d  dtaris                 4     11  0
006800981118      *----------------------------------
006900060623     d Fidn37DS      e ds
007000060622
007100060622     d Trul06DS      e ds
007200060622     d  LIN                    1     90  0    DIM(30)                           P.O. COMODO
007300060622
007400060622     d KPJBA         e ds
007500060622      *
007600060623      * -
007700060623     d TIBS02ds      e ds
007800060622      * - Parametri x Controllo profilo utenti
007900060622     d TIBS34ds      e ds
008000060622      * - Ds di riferimento al file esterno AZUTE00F
008100060622     d AZUTEds       e ds                  extname(AZUTE00F)
008200060622      * - Ds per dati organigramma
008300060622     d dDATIUTE      e ds
008400060622
008500060622     d FNLV50DS      e ds
008600981216      *
008700060623     d Dcch          e ds
008800060623      *
008900060622     d                SDS
009000060622     d  VTCPGM                 1     10
009100981118
009200981118      *------------------------------------------------------------------------*
009300981118      *---------------  C A L C O L O  ----------------------------------------*
009400981118      *------------------------------------------------------------------------*
009500060622     c                   exsr      INIZIO
009600981118
009700981118     C     for01         tag
009800981118
009900060622     c                   write     FI36t01
010000060622     c                   write     FI36z01
010100060622     c                   exfmt     FI36d01
010200981118
010300981119     c                   setoff                                       404128
010400981118     c                   clear                   v1cmsg
010500981118
010600981118      * f3=Fine
010700981118     c   kc              goto      fine
010800060622
010900060622      * f18=Cambio P.O. in gestione
011000060622     c   ks              eval      *in05 = *on
011100060622     c   ks              goto      for01
011200060622
011300060622      * 05 on --> controllo P.O. in gestione
011400060622     c     *in05         ifeq      *on
011500060622     C                   exsr      ctrfgs
011600060622     c   90              goto      for01
011700060622     c                   eval      *in05 = *off
011800060622     c                   end
011900060622
012000981118
012100981118      * Controlli
012200981118     c                   exsr      ctrd01
012300981118
012400981118      * Se non ho confermato o ci sono errori riemetto video
012500981118     c                   IF        *INKF = *ON  and  *IN28 = *OFF
012600981118     c                   exsr      SUB_confer
012700981118     c   28              goto      for01
012800981118     c                   ELSE
012900981118     c                   goto      for01
013000981118     c                   ENDIF
013100981118
013200981118     c     fine          tag
013300060622
013400060622     c                   Eval      *INLR = *ON
013500060622      *****************************************************************
013600060622      * Controllo il P.O. in gestione
013700060622      *****************************************************************
013800060622     c     Ctrfgs        begsr
013900060622      *
014000060622     c                   setoff                                       90
014100060622     c                   clear                   v1dfgs
014200060622     c                   clear                   Fnlv50ds
014300060622     c                   movel     Knmus         D50pru
014400060622     c                   movel     V1cfgs        D50fgs
014500060622     c                   Call      'FNLV50R'
014600060622     c                   Parm                    Fnlv50ds
014700060622      *
014800060622     c                   if        D50err <> *blanks
014900060622     c                   movel     D50msg        V1cmsg
015000060622     c                   seton                                        9028
015100060622     c                   else
015200060622      *
015300060622     c                   EXSR      CARL6
015400060622      * decodifica
015500060622     c     v1cfgs        CHAIN     azorg                              31
015600060622     c  n31              MOVEL     orgdes        v1dfgs
015700060622     c                   endif
015800060622      *
015900060622     c                   endsr
016000981118      **********************************************************************
016100981118      * CONTROLLI VIDEO 1
016200981118      **********************************************************************
016300060623     c     CTRD01        BEGSR
016400981118
016500981118     c                   SELECT
016600981118      *
016700060623     c                   WHEN      V1Cfil = *zeros  and  V1Cnca = *zeros
016800981118      *
016900060623     c                   WHEN      (V1Cfil = *zeros  and  V1Cnca > *zeros)  or
017000060623     c                             (V1Cfil > *zeros  and  V1Cnca = *zeros)
017100060623     c                   movel     MSG(2)        V1Cmsg
017200060623     c                   eval      *in28 = *on
017300981118      *
017400060623     c                   WHEN      V1Caac = *zeros
017500060623     c                   movel     MSG(2)        V1Cmsg
017600060623     c                   eval      *in28 = *on
017700981118      *
017800060623     c                   OTHER
017900060623      *       sistemo anno di due cifre
018000060623     c                   if        V1Caac < 100  and  V1Caac > 60
018100060623     c                   ADD       1900          V1Caac
018200060623     c                   endif
018300060623     c                   if        V1Caac < 100  and  V1Caac <= 60
018400060623     c                   ADD       2000          V1Caac
018500060623     c                   endif
018600981118      *       Controllo C.A.
018700060623     c                   z-add     V1Caac        kaac
018800060623     c                   z-add     V1Cfil        kfil
018900060623     c                   z-add     V1Cnca        knca
019000060623     c     Kdct          CHAIN     FNDCT000                           28
019100981118      *
019200060623     c                   IF        *in28 = *on
019300060623     c                   movel     MSG(2)        V1Cmsg
019400060623     c                   Endif
019500060623      * verifico se pratica assicurativa
019600060623      *
019700060623     c                   IF        DCTfpr <> 'P' and not *in28
019800060623     c                   movel     MSG(7)        V1Cmsg
019900060623     c                   eval      *in28 = *on
020000060623     c                   Endif
020100060622      * verifico la fase
020200060622      * se minore della fase 400
020300060623     c                   IF        DCTfca < wfpad and not *in28
020400060623     c                   movel     MSG(3)        V1Cmsg
020500060623     c                   eval      *in28 = *on
020600060623     c                   Endif
020700060622      * verifico se CA chiusa
020800060623     c                   IF        DCTfca = wfchi  and not *in28
020900060623      * controllo ca di chiusura
021000060623     c                   exsr      chcch
021100060623      *
021200060623     c                   If        §cchima = 'S'
021300060623     c                   movel     MSG(4)        V1Cmsg
021400060623     c                   eval      *in28 = *on
021500060623     c                   Endif
021600060623     c                   endif
021700060623      * verfico se CA con evento
021800060623     c                   IF        DCTnev > *zeros and not *in28
021900060623     c                   movel     MSG(6)        V1Cmsg
022000060623     c                   eval      *in28 = *on
022100060623     c                   ENDIF
022200060627      * verfico se CA Avaria
022300060628     c                   IF        not *in28 and
022400060628     c                                (DCTtad = '03' or DCTtad = '23')
022500060627     c                   movel     MSG(8)        V1Cmsg
022600060627     c                   eval      *in28 = *on
022700060627     c                   ENDIF
022800060628      * verfico se CA Mancanza per disconoscimento firma o perdita LDV
022900060628     c                   IF        not *in28 and
023000060628     c                                (DCTtad = '24' or DCTtad = '25')
023100060628     c                   movel     MSG(9)        V1Cmsg
023200060628     c                   eval      *in28 = *on
023300060628     c                   ENDIF
023400981118      *
023500060623     c                   ENDSL
023600981118      *
023700060623     c                   eval      *in40 =  (*IN28 = *ON)
023800981118     c   28              goto      Ectrd01
023900981118
024000981118      * Controllo data stampa per ristampa
024100981118     C                   IF        V1Cdts > *zeros
024200981118     C                   clear                   WLBDAT
024300981118     C                   z-add     V1Cdts        G02DAT
024400981118     C                   CALL      'XSRDA8'
024500981118     C                   PARM                    WLBDAT
024600981118     C                   IF        G02ERR = '1'
024700981119     C                   seton                                        4128
024800981118     C                   movel     MSG(1)        V1CMSG
024900981118   X C                   ELSE
025000981118     C                   move      G02INV        WV1Cdts
025100981118     C                   z-add     G02DAT        V1Cdts
025200981118     C                   ENDIF
025300981118     C                   ENDIF
025400981118     c   28              goto      Ectrd01
025500981118
025600981118      * Controlli di congruità
025700981118     c                   IF        V1Cnca > 0  and
025800981118     c                               (V1Ctot = 'S'  or  V1Cdts > *zeros)
025900981118     C                   seton                                        4028
026000981118     C                   movel     MSG(5)        V1CMSG
026100981118     c                   ELSE
026200981118     c                   IF        V1Ctot = 'S'  and  V1Cdts > *zeros
026300981118     C                   seton                                        4028
026400981118     C                   movel     MSG(5)        V1CMSG
026500981118     C                   ENDIF
026600981118     C                   ENDIF
026700981118
026800981118     c     Ectrd01       ENDSR
026900981118      *****************************************************************
027000981118      * AGGIORNO ARCHIVI
027100981118      *****************************************************************
027200981118     C     SUB_confer    BEGSR
027300981118
027400981118     c                   clear                   kpjbu
027500981118      *
027600981118     c                   SELECT
027700981118      *
027800981118      * Richiamo PGM di stampa richieste
027900981119     c                   when      V1Ctot = 'S'
028000060627     c                   clear                   param
028100060627     c                   z-add     v1cfgs        poges
028200060627     c                   movel(p)  param         Kpjbu
028300060623     c                   CALL      'FIDN37R1'
028400981118     c                   PARM                    KPJBA
028500981118      *
028600981118      * Richiamo PGM di ristampa lettera
028700981119     c                   when      V1Cdts > *zeros
028800060627     c                   clear                   param
028900060627     c                   z-add     v1cfgs        poges
029000060627     c                   z-add     WV1Cdts       dtaris
029100060627     c                   movel(p)  param         Kpjbu
029200060623     c                   CALL      'FIDN37R2'
029300981118     c                   PARM                    KPJBA
029400981118      *
029500981119      * Stampa e aggiorna singola C. A.
029600981119     c                   when      V1Cnca > *zeros
029700981119     c                   EXSR      PRTAGG
029800060630     c                   If        o37err = 'E'
029900060630     c                   seton                                        2840
030000060630     c                   movel     msg(10)       v1cmsg
030100060630     c                   endif
030200981119      *
030300981118     c                   ENDSL
030400981118      *
030500981118     c                   ENDSR
030600981118      *****************************************************************
030700981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA E AGGIORNO C.A.
030800981118      *****************************************************************
030900981119     C     PRTAGG        BEGSR
031000981118      *
031100060623     c                   clear                   FIDN37DS
031200060623      *
031300060623     c                   movel     V1Cfgs        I37fgs
031400981119      *
031500060623     c                   z-add     DCTaac        I37aac
031600060623     c                   z-add     DCTfil        I37fil
031700060623     c                   z-add     DCTnca        I37nca
031800060623
031900060623     c                   Select
032000060623
032100060623      * verifico il tipo denuncia da stampare in base al tipo anomalia
032200060623      * prevista autodenuncia per ammanchi prima della consegna
032300060623     c                   when      Dcttad = '02'
032400060623     c                   eval      I37tde = '1'
032500060623      * prevista autodenuncia per ammanchi dopo la consegna
032600060623     c                   when      Dcttad = '22'
032700060623     c                   eval      I37tde = '2'
032800060623      * prevista autodenuncia per mancanze
032900060628     c                   when      Dcttad = '01' or Dcttad = '21'
033000060623     c                   eval      I37tde = '3'
033100060623     c                   endsl
033200060703      * come ha detto MARINONI la stampa estemporanea non deve controllare se il beneficiario
033300060703      * appartiene al P.O. gestione
033400060703
033500060703     c*********          eval      i37mod = 'G'
033600981119      *
033700060623     c                   movel(P)  FIDN37DS      KPJBU
033800981119      *
033900060623     c                   CALL      'FIDN37R3'
034000981119     c                   PARM                    KPJBA
034100981119      *
034200060630     c                   movel(P)  kpjbu         fidn37ds
034300981119      *
034400981119     c                   ENDSR
034500981119      *****************************************************************
034600981119      * ROUTINE INIZIALE
034700981119      *****************************************************************
034800060622     C     INIZIO        begsr
034900060622      *
035000981118     C     *ENTRY        PLIST
035100981118     C                   PARM                    KPJBA
035200981118      *
035300981118      * data e ora
035400981118     C                   TIME                    W0140
035500981118      * UDATE IN GGMMAAAA
035600981118     C                   MOVE      W0140         WDTGIO
035700981119      *
035800981118     C                   Z-ADD     WDTGIO        G02DAT
035900981118     C                   MOVEL     *BLANK        G02ERR
036000981118     C                   CALL      'XSRDA8'
036100981118     C                   PARM                    WLBDAT
036200981119      * UDATE IN AAAAMMGG
036300981118     C                   MOVEL     G02INV        DATEU
036400981118      *
036500981216      * Preimposto dati video
036600981118     c                   move      wdtgio        V1Caac
036700981118     c                   move      'N'           V1Ctot
036800981216      *
036900060622      *
037000060622     c     *dtaara       define    §azute        AZUTEds
037100060622     c     *dtaara       define    §datiute      dDATIUTE
037200060622     c                   in(E)     *dtaara
037300060622     c                   if        %ERROR or RSUT = *blanks
037400060622     c                   clear                   Tibs34Ds
037500060622     c                   call      'TIBS34R'
037600060622     c                   parm                    Tibs34Ds
037700060622     c                   in        *dtaara
037800060622     c                   endif
037900060622
038000060622      * IMPOSTO IL P.O. IN GESTIONE
038100060622     c                   movel     dutpou        v1cfgs
038200060622     c                   movel     dutdpo        v1dfgs
038300060622
038400060622      * se è un secondo livello non si può fare cambio p.o. gestione
038500060622
038600060622     c     dutlpo        ifeq      '2'
038700060622     c                   seton                                        06
038800060622     c                   endif
038900060622      *
039000060622      * carico L6
039100060622      *
039200060622     C                   exsr      CARL6
039300981118      *
039400060622     c     KDCT          klist
039500060622     c                   Kfld                    kaac
039600060622     c                   Kfld                    kfil
039700060622     c                   Kfld                    knca
039800981118      *
039900060622     c                   Endsr
040000060622      *---------------------------------------------------------------*
040100060622      * Carico schiera L6 da tabella £6
040200060622      *---------------------------------------------------------------*
040300060622     c     carl6         Begsr
040400060622
040500060622     c                   clear                   L6
040600060622     c                   clear                   Trul06ds
040700060622     c                   move      '£6'          D06cod
040800060622     c                   movel     V1Cfgs        D06key
040900060622     c                   Movel     Trul06ds      Kpjbu
041000060622      *
041100060622     c                   Call      'TRUL06R'
041200060622     c                   Parm                    KPJBA
041300060622      *
041400060622     c                   Movel     KPJBU         TRUL06DS
041500060622     c                   Movea     LIN           L6
041600060622      *
041700060622     c                   endsr
041800060623      *---------------------------------------------------------------*
041900060623      * Controllo causale di chiusura
042000060623      *---------------------------------------------------------------*
042100060623     c     chcch         Begsr
042200060623
042300060623      *
042400060623     c                   clear                   DCCH
042500060623      *
042600060623     c                   clear                   TIBS02DS
042700060623     c                   MOVEL     'C'           t02mod
042800060623     c                   MOVEL     knsif         t02sif
042900060623     c                   MOVEL     'CCH'         t02cod
043000060623     c                   MOVEL(P)  dctcch        t02ke1
043100060623      *
043200060623     c                   CALL      'TIBS02R'
043300060623     c                   PARM                    KPJBA
043400060623     c                   PARM                    TIBS02DS
043500060623      *
043600060623     c                   if        t02err = *BLANKS
043700060623     c                   MOVEL     T02UNI        DCCH
043800060623     c                   endif
043900060623      *
044000060623     c                   endsr
044100981119      *---------------------------------------------------------------------------------------------
044200981118** MSG  Lungh. 78                                                            *
044300981118Inseriti dati non validi                                                       1
044400981118Non esiste nessuna C. A. con il numero immesso                                 2
044500060622La C.A. non è nella giusta fase                                                3
044600060623La C.A. risulta chiusa Non è possibile stampare autodichiarazioni              4
044700981118Effettuare solo un tipo di selezione                                           5
044800060623La C.A. è legata ad un evento                                                  6
044900060623La C.A. non è una pratica assicurativa                                         7
045000060627La C.A. ha tipo anomalia Avaria                                                8
045100060628La C.A. ha tipo anomalia Mancanza non prevista per Autodichiarazione           9
045200060630Il cliente beneficiario della C.A. non appartiene al P.O. gestione             10
