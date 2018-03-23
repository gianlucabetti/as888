000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400081008      *  Descrizione   :  conferma ricezione stampa tariffe autisti AFF/DEF
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700160408     Faitrs01l  uf   e           k disk
000800160408     Fficna7D1  CF   E             WORKSTN
000900000000      *
001000940926     D*
001100000000     D KPJBA         E DS
001200000000     D CNCR80        E DS
001300131008     D tibs42ds      E DS
001400131008     D uteautds      E DS
001500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600940926     D*
001700000000     D WLBDAT          DS
001800940926     D  G02DAT                 1      8  0
001900940926     D  G02INV                 9     16  0
002000940926     D  G02ERR                17     17
002100940926     D  G02TGI                18     22  0
002200081008     d dataiso         s               d   datfmt(*iso)
002300081008     d dataeur         s               d   datfmt(*eur)
002400090914      * DS per impostare default se con pistola o manuale
002500090914     D combts          s              1
002600090914     c* ds per sottocampare il barcode della spedizione
002700090914     D ds_pis          DS
002800160408     D  ds_societa                    3
002900090914     D  ds_pisfil                     3  0
003000160408     D  ds_piscont                    7  0
003100160408     D  ds_pis0                       7  0
003200090914     D  ds_pisdata                    8  0
003300090914     D  ds_chkdgt                     1  0
003400160408     d savchk          s                   like(o284ckd)
003500160408     d cbarre28        s             28
003600090914     c*controllo check digits
003700160408     D trul28dsx     E DS
003800081008      *__________________________________________________________________
003900000000     C     *ENTRY        PLIST
004000000000     C                   PARM                    KPJBA
004100081008
004200081008     C                   time                    w0120            14 0
004300081008     C                   move      w0120         wdat              8 0
004400081008      * data da impostare su data stampa e convalida se lancio = 'D'
004500081008     c                   move      wdat          dataeur
004600081008     c                   move      dataeur       dataiso
004700081008     c                   move      dataiso       dataoggi          8 0
004800090505     c                   move      dataeur       vidudat
004900000000      *
005000000000      *--------------------------------------------------------------*
005100000000      *             M A I N      L I N E
005200000000      *--------------------------------------------------------------*
005300000000     C                   Z-ADD     1             CODUT
005400000000     C                   CALL      'X§PARUT'
005500000000     C                   PARM                    UT§DSE
005600000000     C                   MOVEL     RAGUT         RSUT             20
005700000000     C                   MOVEL     RAGUT         RSUT2            20
005800000000     C                   MOVEL     REC80         CNCR80
005900000000      *
006000000000     C                   EXSR      DEFCAM                                       INIZIALIZZ.
006100000000      *
006200000000      * Inizializzo i campi a video
006300000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
006400020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
006500000000      *
006600000000      * Emetto il formato video
006700000000     C     EMETTO        TAG
006800020308     C                   SETOFF                                       90
006900090914     c                   if        combts = 'L'
007000090914     c                   eval      v03f2 = 'F2=Manuale'
007100090914     c                   else
007200090914     c                   eval      v03f2 = 'F2=Pistola'
007300090914     c                   end
007400090914      *
007500081008     C                   EXFMT     cna7D01
007600000000      *
007700000000     C   KC              GOTO      FINE                                         F3=USCITA
007800000000      *
007900000000      * Eseguo i  controlli sui campi immessi a video
008000090914     c* serve per il posizionamento sulla spedizione o sul barcode
008100090914     C                   if        *inkb
008200160411     c                   setoff                                       42
008300090914     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
008400090914     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
008500090914     C                   clear                   v03pis
008600090914     c                   if        combts = 'L'
008700090914     c                   movel     *blanks       combts
008800090914     c                   eval      v03f2 = 'F2=Pistola'
008900090914     c                   setoff                                       73
009000090914     C                   GOTO      EMETTO
009100090914     c                   else
009200090914     c                   movel     'L'           combts
009300090914     c                   eval      v03f2 = 'F2=Manuale'
009400090914     c                   seton                                        73
009500090914     C                   GOTO      EMETTO
009600090914     c                   end
009700090914     C                   END
009800090514     C                   EXSR      CONTR
009900131025      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
010000131025     c                   clear                   tibs42ds
010100160408     c                   if        ds_pisfil <> *zero
010200160408     c                   move      ds_pisfil     i42pge
010300160408     c                   else
010400160408     c                   movel     001           i42pge
010500160408     c                   endif
010600131025     c                   call      'TIBS42R'
010700131025     c                   parm                    tibs42ds
010800131025     c                   movel     o42uni        uteautds
010900131025     c                   if        §affcrs <> 'S'
011000131025     c                   goto      fine
011100131025     c                   endif
011200091001     c                   if        not *in90 and not *inkf and combts <> 'L'
011300091001     c                   seton                                        90
011400091001     c                   endif
011500000000      *
011600000000      * Per errore generico riemetto il formato video
011700000000     C   90              GOTO      EMETTO
011800000000      *
011900081008      * Richiamo programma di conferma
012000091001     c                   if        *inkf or combts = 'L'
012100160408     c     cdpa1v        chain     aitrs01l
012200160408     c                   if        %found(aitrs01l)
012300160408     c                   z-add     dataoggi      trsdrc
012400160408     c                   update    aitrs000
012500091007      * solo se a mano e non con la pistola
012600091007     c                   if        combts <> 'L'
012700090608     c                   clear                   viddal
012800090608     c                   clear                   cdpa1v
012900091007     c                   end
013000091012     c                   clear                   v03pis
013100081008     c                   goto      emetto
013200081008     c                   endif
013300020308     c                   endif
013400000000      *
013500000000     C     FINE          TAG
013600000000     C                   SETON                                        LR
013700000000      *---------------------------------------------------------------*
013800000000      * Definizioni/dichiarazioni iniziali                            *
013900000000      *---------------------------------------------------------------*
014000000000     C     DEFCAM        BEGSR
014100090914     c* dove andrà il barcode della spedizione
014200090914     C                   MOVEL     'L'           comBTS
014300090914     c     combts        comp      'L'                                    73
014400000000     C                   ENDSR
014500000000      *--------------------------------------------------------------*
014600000000      * Routine per i controlli sui campi del formato video          *
014700000000      *--------------------------------------------------------------*
014800000000     C     CONTR         BEGSR
014900000000      *
015000160411     c                   movea     '00000000'    *in(41)
015100091001     c                   clear                   ds_pis
015200090914     c   73              if        v03pis = *blank
015300090914     c                   seton                                        1190
015400090914     C                   GOTO      ENDCTL
015500090914     c                   else
015600090914     c                   movel     v03pis        ds_pis
015700091012     c                   if        combts =  'L'
015800160408     c                   move      ds_piscont    cdpa1v
015900160411     c                   move      ds_pisdata    dataiso
016000160411     c                   move      dataiso       dataeur
016100160411     c                   move      dataeur       vidDAL
016200160408     c                   movel     v03pis        cbarre28
016300090914      *
016400090914     c* controllo il check digits
016500090914     c                   exsr      srchkdgt
016600090914      *
016700090914     c                   if        *in10
016800160408     C*                  GOTO      ENDCTL
016900090914     c                   end
017000090914      *
017100090914     c                   end
017200090914     c                   end
017300000000      *
017400000000      * Controlli di congruenza
017500160408      * Se immesso il numero contratto
017600020307     C     CDPA1V        IFeq      *zero
017700020307     C                   SETON                                        9045
017800000000     C   90              GOTO      ENDCTL
017900000000     C                   ENDIF
018000000000      *
018100000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
018200020307     C     VIDDAL        ifeq      0
018300020308     C                   SETON                                        4390
018400000000     C                   GOTO      ENDCTL
018500000000     C                   END
018600000000      * Controllo data iniziale
018700000000     C     VIDDAL        IFGT      0
018800940926     C                   Z-ADD     VIDDAL        G02DAT
018900000000     C                   MOVEL     *BLANK        G02ERR
019000940926     C                   CALL      'XSRDA8'
019100000000     C                   PARM                    WLBDAT
019200000000     C     G02ERR        IFEQ      '1'
019300090505     C                   SETON                                        44  90
019400000000     C                   GOTO      ENDCTL
019500000000     C                   ENDIF
019600020307     C                   Z-ADD     G02INV        DALRIB            8 0
019700020308     C                   Z-ADD     G02dat        viddal
019800020307     c                   endif
019900090505      * Controllo data conferma ricezione
020000090505     C     VIDudat       ifeq      0
020100160411     C                   SETON                                        4190
020200090505     C                   GOTO      ENDCTL
020300090505     C                   END
020400090505     C                   IF        vidudat <> wdat
020500090505     C                   Z-ADD     VIDudat       G02DAT
020600090505     C                   MOVEL     *BLANK        G02ERR
020700090505     C                   CALL      'XSRDA8'
020800090505     C                   PARM                    WLBDAT
020900090505     C     G02ERR        IFEQ      '1'
021000160411     C                   SETON                                        41  90
021100090505     C                   GOTO      ENDCTL
021200090505     C                   ENDIF
021300090505     C                   Z-ADD     G02INV        DAtaoggi
021400090505     C                   Z-ADD     G02dat        vidudat
021500090505     c                   endif
021600090505     c                   if        dataoggi < dalrib
021700160411     C                   SETON                                        42  90
021800090505     C                   GOTO      ENDCTL
021900090505     C                   ENDIF
022000160408     c                   setoff                                       48
022100160411     c                   if        cdpa1v > 0
022200160408     c     cdpa1v        chain     aitrs01l
022300160408     c                   if        %found(aitrs01l)
022400160411     c                   if        trsdrc > 0 or
022500160411     c                             trsdfc > 0
022600160408     C                   seton                                        48  90
022700081008     C                   endif
022800160408     C                   endif
022900160411     C                   endif
023000000000      *
023100000000     C     ENDCTL        ENDSR
023200090914     C*-----------------------------------------------------*
023300090914     C* controllo check digits
023400090914     C*-----------------------------------------------------*
023500090914     C     srchkdgt      BEGSR
023600160408     c                   movel(p)  cbarre28      cbarre29         29
023700160408     C                   clear                   trul28dsx
023800160408     c                   eval      i284std = 'E13'
023900160408     c                   eval      i284cod = cbarre28
024000160408     c                   call      'TRUL28R4'
024100160408     c                   parm                    trul28dsx
024200160408     c* se reperito barcode imposto i campi in stampa
024300160408     c                   if        o284err = *blanks
024400160408     c                   move      O284CKD       cbarre29
024500160408     c                   end
024600160408      *
024700090914     c* se reperito barcode imposto i campi
024800160408     c*                  if        o284err <> *blanks or o284ckd <> ds_pis
024900160408     c*                  seton                                        1090
025000160408     c*                  end
025100090914     C                   ENDSR
