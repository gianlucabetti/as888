000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400081008      *  Descrizione   :  conferma ricezione stampa tariffe autisti AFF/DEF
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700000000     FAZORG01L  IF   E           K DISK
000800020402     Ftntbe01L  IF   E           K DISK
000900021203     Ffiapd01l  if   e           k disk
001000081008     Ffiadt01l  uf   e           k disk
001100081008     Fficna7D   CF   E             WORKSTN
001200000000      *
001300940926     D*
001400000000     D KPJBA         E DS
001500000000     D CNCR80        E DS
001600131008     D tibs42ds      E DS
001700131008     D uteautds      E DS
001800000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900940926     D*
002000030203     D tipo            s              1
002100940926     D*
002200000000     D WLBDAT          DS
002300940926     D  G02DAT                 1      8  0
002400940926     D  G02INV                 9     16  0
002500940926     D  G02ERR                17     17
002600940926     D  G02TGI                18     22  0
002700081008     d dataiso         s               d   datfmt(*iso)
002800081008     d dataeur         s               d   datfmt(*eur)
002900090914      * DS per impostare default se con pistola o manuale
003000090914     D combts          s              1
003100090914     c* ds per sottocampare il barcode della spedizione
003200090914     D ds_pis          DS
003300090914     D  ds_pisfil                     3  0
003400090914     D  ds_piscod                     4  0
003500090914     D  ds_pisdata                    8  0
003600090914     D  ds_chkdgt                     1  0
003700090914     d savchk          s                   like(o28ckd)
003800090914     d barcode         s             15
003900090914     c*controllo check digits
004000090914     D trul28ds      E DS
004100081008      *__________________________________________________________________
004200000000     C     *ENTRY        PLIST
004300000000     C                   PARM                    KPJBA
004400081008
004500081008     C                   time                    w0120            14 0
004600081008     C                   move      w0120         wdat              8 0
004700081008      * data da impostare su data stampa e convalida se lancio = 'D'
004800081008     c                   move      wdat          dataeur
004900081008     c                   move      dataeur       dataiso
005000081008     c                   move      dataiso       dataoggi          8 0
005100090505     c                   move      dataeur       vidudat
005200021203     c     kapd          klist
005300030203     c                   kfld                    tipo
005400021203     c                   kfld                    kpdr
005500081008     c                   movel     'D'           tipo
005600081008     c     kadt          klist
005700081008     c                   kfld                    adtpdr
005800081008     c                   kfld                    adtprg
005900000000      *
006000000000      *--------------------------------------------------------------*
006100000000      *             M A I N      L I N E
006200000000      *--------------------------------------------------------------*
006300000000     C                   Z-ADD     1             CODUT
006400000000     C                   CALL      'X§PARUT'
006500000000     C                   PARM                    UT§DSE
006600000000     C                   MOVEL     RAGUT         RSUT             20
006700000000     C                   MOVEL     RAGUT         RSUT2            20
006800000000     C                   MOVEL     REC80         CNCR80
006900000000      *
007000000000     C                   EXSR      DEFCAM                                       INIZIALIZZ.
007100000000      *
007200000000      * Inizializzo i campi a video
007300000000     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
007400000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
007500020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
007600000000      *
007700000000      * Emetto il formato video
007800000000     C     EMETTO        TAG
007900020308     C                   SETOFF                                       90
008000090914     c                   if        combts = 'L'
008100090914     c                   eval      v03f2 = 'F2=Manuale'
008200090914     c                   else
008300090914     c                   eval      v03f2 = 'F2=Pistola'
008400090914     c                   end
008500090914      *
008600081008     C                   EXFMT     cna7D01
008700000000      *
008800000000     C   KC              GOTO      FINE                                         F3=USCITA
008900000000      *
010100000000      * Eseguo i  controlli sui campi immessi a video
010200090914     c* serve per il posizionamento sulla spedizione o sul barcode
010300090914     C                   if        *inkb
010400090914     c                   setoff                                       52
010500090914     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
010600090914     C                   clear                   desFIL
010700090914     C                   clear                   desPAD
010800090914     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
010900090914     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
011000090914     C                   clear                   v03pis
011100090914     c                   if        combts = 'L'
011200090914     c                   movel     *blanks       combts
011300090914     c                   eval      v03f2 = 'F2=Pistola'
011400090914     c                   setoff                                       73
011500090914     C                   GOTO      EMETTO
011600090914     c                   else
011700090914     c                   movel     'L'           combts
011800090914     c                   eval      v03f2 = 'F2=Manuale'
011900090914     c                   seton                                        73
012000090914     C                   GOTO      EMETTO
012100090914     c                   end
012200090914     C                   END
012300090514     C                   EXSR      CONTR
012301131025      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
012302131025     c                   clear                   tibs42ds
012303131025     c                   movel     vidfil        i42pge
012304131025     c                   call      'TIBS42R'
012305131025     c                   parm                    tibs42ds
012306131025     c                   movel     o42uni        uteautds
012307131025     c                   if        §affcrs <> 'S'
012308131025     c                   goto      fine
012309131025     c                   endif
012400091001     c                   if        not *in90 and not *inkf and combts <> 'L'
012500091001     c                   seton                                        90
012600091001     c                   endif
012700000000      *
012800020308      * Se non vi sono errori ma non e' premuto F6 emetto video
012900091001     C* N90
013000091001     C*nNkf              SETON                                          90      F6=CONFERMA
013100000000      *
013200000000      * Per errore generico riemetto il formato video
013300000000     C   90              GOTO      EMETTO
013400000000      *
013500081008      * Richiamo programma di conferma
013600091001     c                   if        *inkf or combts = 'L'
013700081008     c     kadt          chain     fiadt01l
013800081008     c                   if        %found(fiadt01l)
013900090402     c                   z-add     dataoggi      adtdrc
014000081008     c                   update    fiadt000
014100091007      * solo se a mano e non con la pistola
014200091007     c                   if        combts <> 'L'
014300090608     c                   clear                   viddal
014400090608     c                   clear                   vidfil
014500090608     c                   clear                   desfil
014600090608     c                   clear                   despad
014700090608     c                   clear                   cdpa1v
014800091007     c                   end
014900091012     c                   clear                   v03pis
015000081008     c                   goto      emetto
015100081008     c                   endif
015200020308     c                   endif
015300000000      *
015400000000     C     FINE          TAG
015500000000     C                   SETON                                        LR
015600000000      *---------------------------------------------------------------*
015700000000      * Definizioni/dichiarazioni iniziali                            *
015800000000      *---------------------------------------------------------------*
015900000000     C     DEFCAM        BEGSR
016000020402     C                   MOVEL     simfel        VIDpog            3 0
016100020402     C* VEDO SE SONO SIMFEL O REMOTO
016200020701     C                   EVAL      VidFil = SimPOU
016300020701     C                   EVAL      VidPOG = SimPOU
016400090914     c* dove andrà il barcode della spedizione
016500090914     C                   MOVEL     'L'           comBTS
016600090914     c     combts        comp      'L'                                    73
016700000000     C                   ENDSR
016800000000      *--------------------------------------------------------------*
016900000000      * Routine per i controlli sui campi del formato video          *
017000000000      *--------------------------------------------------------------*
017100000000     C     CONTR         BEGSR
017200000000      *
017300020308     c                   movea     '000000'      *in(41)
017400090505     c                   setoff                                       485152
017500091001     c                   clear                   ds_pis
017600090914     c   73              if        v03pis = *blank
017700090914     c                   seton                                        1190
017800090914     C                   GOTO      ENDCTL
017900090914     c                   else
018000090914     c                   movel     v03pis        ds_pis
018100091012     c*******            if        vidfil = 0 and cdpa1v = 0 and vidDAL = 0
018200091012     c                   if        combts =  'L'
018300090914     c                   move      ds_pisfil     vidfil
018400090914     c                   move      ds_piscod     cdpa1v
018500090914     c                   move      ds_pisdata    vidDAL
018600090914      *
018700090914     c* controllo il check digits
018800090914     c                   exsr      srchkdgt
018900090914      *
019000090914     c                   if        *in10
019100090914     C                   GOTO      ENDCTL
019200090914     c                   end
019300090914      *
019400090914     c                   else
019500090914     c                   if        vidfil<>ds_pisFIL or
019600090914     c                             cdpa1v<>ds_pisCOD or vidDAL<>ds_pisdata
019700091012     c******             seton                                        1190
019800091012     C******             GOTO      ENDCTL
019900090914     c                   end
020000090914      *
020100090914     c                   end
020200090914     c                   end
020300000000      * Verifico il codice filiale
020400000000     C                   MOVE      *BLANKS       DESFIL
020500000000     C     VIDFIL        IFGT      0
020600000000     C     VIDFIL        CHAIN     AZORG01L                           41
020700000000     C  N41              MOVEL     ORGDES        DESFIL
020800000000     C   41              SETON                                        90
020900000000     C   41              GOTO      ENDCTL
021000020308     c                   else
021100020308     C                   SETON                                        9041
021200020308     C                   GOTO      ENDCTL
021300000000     C                   ENDIF
021400000000      *
021500000000      * Controlli di congruenza
021600020312      * Se immesso il cd.padroncino
021700020307     C     CDPA1V        IFeq      *zero
021800020307     C                   SETON                                        9045
021900000000     C   90              GOTO      ENDCTL
022000000000     C                   ENDIF
022100000000      *
022200000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
022300020307     C     VIDDAL        ifeq      0
022400020308     C                   SETON                                        4390
022500000000     C                   GOTO      ENDCTL
022600000000     C                   END
022700000000      * Controllo data iniziale
022800000000     C     VIDDAL        IFGT      0
022900940926     C                   Z-ADD     VIDDAL        G02DAT
023000000000     C                   MOVEL     *BLANK        G02ERR
023100940926     C                   CALL      'XSRDA8'
023200000000     C                   PARM                    WLBDAT
023300000000     C     G02ERR        IFEQ      '1'
023400090505     C                   SETON                                        44  90
023500000000     C                   GOTO      ENDCTL
023600000000     C                   ENDIF
023700020307     C                   Z-ADD     G02INV        DALRIB            8 0
023800020308     C                   Z-ADD     G02dat        viddal
023900020307     c                   endif
024000090505      * Controllo data conferma ricezione
024100090505     C     VIDudat       ifeq      0
024200090505     C                   SETON                                        5190
024300090505     C                   GOTO      ENDCTL
024400090505     C                   END
024500090505     C                   IF        vidudat <> wdat
024600090505     C                   Z-ADD     VIDudat       G02DAT
024700090505     C                   MOVEL     *BLANK        G02ERR
024800090505     C                   CALL      'XSRDA8'
024900090505     C                   PARM                    WLBDAT
025000090505     C     G02ERR        IFEQ      '1'
025100090505     C                   SETON                                        51  90
025200090505     C                   GOTO      ENDCTL
025300090505     C                   ENDIF
025400090505     C                   Z-ADD     G02INV        DAtaoggi
025500090505     C                   Z-ADD     G02dat        vidudat
025600090505     c                   endif
025700090505     c                   if        dataoggi < dalrib
025800090505     C                   SETON                                        52  90
025900090505     C                   GOTO      ENDCTL
026000090505     C                   ENDIF
026100090505      *
026200020402     c                   movel     vidfil        kpdr              7 0
026300020402     c                   move      cdpa1v        kpdr
026400021203     c     kapd          chain     fiapd01l
026500090507     c                   movel     apdrsf        despad
026600081008     c                   setoff                                       474849
026700081008     c                   clear                   trovato           1
026800081008     c     kpdr          setll     fiadt01l
026900081008     c                   do        *hival
027000081008     c     kpdr          reade(n)  fiadt01l
027100081008     c                   if        %eof(fiadt01l)
027200081008     c                   leave
027300081008     c                   endif
027400081008     c                   if        adtatb <> *blank
027500081008     c                   iter
027600081008     c                   endif
027700090514     c*                  if        dalrib >= adtddt and dalrib <= adtdst
027800090514     c                   if        dalrib = adtddt
027900081008     c                   move      'X'           trovato
028000081008     c                   leave
028100081008     c                   endif
028200081008     c                   enddo
028300090402     c                   if        trovato <>*blank  and adtdts = 0 or
028400090402     c                             trovato <>*blank  and adtdcn = 0
028500081008     C                   seton                                        49  90
028600081008     C                   endif
028700090402     c                   if        trovato <>*blank  and adtdrc > 0
028800090514     c                             and fatto = *blank and
028900090514     c                             adtdrc <> dataoggi
029000090514     C                   seton                                        48
029100090514     c                   move      'X'           fatto             1
029200081008     C                   endif
029300090514     c                   if        fatto <> *blank and not *in48
029400090514     c                   move      *blank        fatto
029500090514     c                   endif
029600081008     c                   if        trovato = *blank
029700081008     C                   seton                                        47  90
029800081008     C                   endif
029900000000      *
030000000000     C     ENDCTL        ENDSR
030100090914     C*-----------------------------------------------------*
030200090914     C* controllo check digits
030300090914     C*-----------------------------------------------------*
030400090914     C     srchkdgt      BEGSR
030500090914     c                   movel     ds_pis        barcode
030600090914     C                   clear                   trul28ds
030700090914     c                   eval      i28mod = 'BAR'
030800090914     c                   eval      i28cod = barcode
030900090914     c                   call      'TRUL28R1'
031000090914     c                   parm                    trul28ds
031100090914     c* se reperito barcode imposto i campi
031200090914     c                   if        o28err <> *blanks or  o28cod <> ds_pis
031300090914     c                   seton                                        1090
031400090914     c                   end
031500090914     C                   ENDSR
