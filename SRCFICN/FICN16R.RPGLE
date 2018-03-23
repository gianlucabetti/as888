000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400020402      *  Descrizione   :  conferma ricezione stampa tariffe padroncini
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700000000     FAZORG01L  IF   E           K DISK
000800020402     Ftntbe01L  IF   E           K DISK
000900021203     Ffiapd01l  if   e           k disk
001000020402     Fficn16D   CF   E             WORKSTN
001100000000      *
001200960913     D* SCHIERE PER CARICAMENTO L1
001300940926     D L1              S              3  0 DIM(30)                              FIL GESTITE
001400940926     D*
001500000000     D KPJBA         E DS
001600020701     D* REM                    1      3
001700020701     D* REMFIL                 4      6
001800000000     D CNCR80        E DS
001900000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000131008     D tibs42ds      E DS
002100131008     D uteautds      E DS
002200090904      * DS per impostare default se con pistola o manuale
002300090904     D combts          s              1
002400090904     c* ds per sottocampare il barcode della spedizione
002500090904     D ds_pis          DS
002600090904     D  ds_pisfil                     3  0
002700090904     D  ds_piscod                     4  0
002800090904     D  ds_pisdata                    8  0
002900090904     D  ds_chkdgt                     1  0
003000090904     d*---------------------------------------------------------------*
003100090904     d* campi di comodo                                               *
003200090904     d*---------------------------------------------------------------*
003300090904     d savchk          s                   like(o28ckd)
003400090904     d barcode         s             15
003500090904     c*controllo check digits
003600090904     D trul28ds      E DS
003700940926     D*
003800030203     D tipo            s              1
003900020307     D dsparam         DS
004000020307     D  padr1                         7s 0
004100020307     D  datae                         8s 0
004200030416     D  serv1                         1
004300090407     D  riterr                        1
004400090417     D  dtadrc                        8s 0
004500090514     D  forza                         1
004600940926     D*
004700940926     D* DS PER TRUL06R - CARICAMENTO £1
004800040309     D*DSUL06        E DS                  EXTNAME(TRUL06DS)
004900040309     D* LIN                    1     90  0
005000090407     D*                                    DIM(30)                              SKI COMODO
005100940926     D*
005200000000     D WLBDAT          DS
005300940926     D  G02DAT                 1      8  0
005400940926     D  G02INV                 9     16  0
005500940926     D  G02ERR                17     17
005600940926     D  G02TGI                18     22  0
005700090417     d dataiso         s               d   datfmt(*iso)
005800090417     d dataeur         s               d   datfmt(*eur)
005900000000      *
006000000000     C     *ENTRY        PLIST
006100000000     C                   PARM                    KPJBA
006200021203     c     kapd          klist
006300030203     c                   kfld                    tipo
006400021203     c                   kfld                    kpdr
006500030203     c                   movel     kpjbu         tipo
006600090417     C                   time                    w0120            14 0
006700090417     C                   move      w0120         wdat              8 0
006800090417      * data da impostare su data stampa e convalida se lancio = 'C'
006900090417     c                   move      wdat          dataeur
007000090417     c                   move      dataeur       dataiso
007100090417     c                   move      dataeur       viddrc
007200090417     c                   move      dataiso       dataoggi          8 0
007300030416     c     tipo          comp      'C'                                    49
007400090324     c                   if        tipo = 'C'
007500090324     c                   eval      autcop = 'Cooperativa'
007600090407     c                   eval      desdec = 'In vigore alla data'
007700090324     c                   else
007800090417     c
007900090324     c                   eval      autcop = 'Autotrasp.'
008000090514     c                   eval      desdec = 'Con decorrenza del'
008100090324     c                   end
008200000000      *
008300090904     c* accendo o spengo l'indicatore 73 per gestire in seguito il campo
008400090904     c* dove andrà il barcode della spedizione
008500091007     C  n49              MOVEL     'L'           comBTS
008600090904     c     combts        comp      'L'                                    73
008700090904     C*
008800000000      *--------------------------------------------------------------*
008900000000      *             M A I N      L I N E
009000000000      *--------------------------------------------------------------*
009100000000     C                   Z-ADD     1             CODUT
009200000000     C                   CALL      'X§PARUT'
009300000000     C                   PARM                    UT§DSE
009400000000     C                   MOVEL     RAGUT         RSUT             20
009500000000     C                   MOVEL     RAGUT         RSUT2            20
009600000000     C                   MOVEL     REC80         CNCR80
009700000000      *
009800000000     C                   EXSR      DEFCAM                                       INIZIALIZZ.
009900000000      *
010000090515     c                   do        *hival
010100090904      *
010200000000      * Inizializzo i campi a video
010300000000     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
010400000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
010500020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
010600090904     C                   clear                   v03pis
010700090515     c                   setoff                                       9052
010800000000      *
010900000000      * Emetto il formato video
011000000000     C     EMETTO        TAG
011100090904      *
011200091007     c  n49              if        combts = 'L'
011300090904     c                   eval      v03f2 = 'F2=Manuale'
011400090904     c                   else
011500090904     c                   eval      v03f2 = 'F2=Pistola'
011600090904     c                   end
011700090904      *
011800090515     C                   EXFMT     cn16D01
011900000000      *
012000090904     c                   setoff                                       90
012100000000     C   KC              GOTO      FINE                                         F3=USCITA
012200090904      *
012300090904      *
012400090904     C* F2-Inserimento spedizione da manuale a pistola o viceversa
012500090904     c* serve per il posizionamento sulla spedizione o sul barcode
012600091007     C  n49              if        *inkb
012700090904     c                   setoff                                       52
012800090904     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
012900090904     C                   clear                   desFIL
013000090904     C                   clear                   desPAD
013100090904     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
013200090904     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
013300090904     C                   clear                   v03pis
013400090904     c                   if        combts = 'L'
013500090904     c                   movel     *blanks       combts
013600090904     c                   eval      v03f2 = 'F2=Pistola'
013700090904     c                   setoff                                       73
013800090904     C                   GOTO      EMETTO
013900090904     c                   else
014000090904     c                   movel     'L'           combts
014100090904     c                   eval      v03f2 = 'F2=Manuale'
014200090904     c                   seton                                        73
014300090904     C                   GOTO      EMETTO
014400090904     c                   end
014500090904     C                   END
014600000000      *
014700000000      * Eseguo i  controlli sui campi immessi a video
014800000000     C                   EXSR      CONTR
014900000000      *
015000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
015100020308     C  N90
015200090514     CanNki
015300020308     CanNkf              SETON                                          90      F6=CONFERMA
015400000000      *
015500000000      * Per errore generico riemetto il formato video
015600000000     C   90              GOTO      EMETTO
015700000000      *
015800020307      * Richiamo programma di stampa
015900090514     c                   if        *inkf or *inki
016000090515     c                   if        *inki
016100090515     c                   eval      forza = '1'
016200090515     c                   else
016300090515     c                   clear                   forza
016400090515     c                   setoff                                       52
016500090515     c                   end
016600020307     c                   z-add     cdpa1v        padr1
016700020312     c                   movel     vidfil        padr1
016800020312     c                   z-add     dalrib        datae
016900030416     c                   move      vidtsr        serv1
017000090407     c                   clear                   riterr
017100020307     c                   movel     dsparam       kpjbu
017200030203     c                   if        tipo = 'A'
017300090420     c                   if        simpou = 046
017400090109     C                   CALL      'FICN17C'
017500000000     C                   PARM                    KPJBA
017600090420     c                   else
017700090420     C                   CALL      'FICN17R2'
017800090420     C                   PARM                    KPJBA
017900090420     c                   end
018000090407     c                   movel     kpjbu         dsparam
018100090407     c                   if        riterr <> ' '
018200090514     c                   if        riterr =  '2'
018300090515     c                   seton                                        9052
018400090515     c                   else
018500090515     c                   seton                                        9046
018600090514     c                   end
018700090407     C                   GOTO      EMETTO
018800090407     c                   end
018900030203     c                   else
019000030203     C                   CALL      'FICN17R1'
019100030203     C                   PARM                    KPJBA
019200030203     c                   endif
019300090420     c                   movel     kpjbu         dsparam
019400020308     c                   endif
019500090515     c                   enddo
019600000000      *
019700000000     C     FINE          TAG
019800000000     C                   SETON                                        LR
019900000000      *---------------------------------------------------------------*
020000000000      * Definizioni/dichiarazioni iniziali                            *
020100000000      *---------------------------------------------------------------*
020200000000     C     DEFCAM        BEGSR
020300940926     C***
020400940926     C* CARICO TABELLA FILIALI GESTITE £1
020500940926     C***
020600040309     C*                  CLEAR                   DSUL06
020700040309     C*                  MOVE      '£1'          D06COD
020800040309     C*    SIMFEL        IFGT      0
020900040309     C*                  MOVEL     SIMFEL        D06KEY
021000020402     C                   MOVEL     simfel        VIDpog            3 0
021100040309     C*                  ENDIF
021200040309     C*                  MOVEL     DSUL06        KPJBU
021300940926     C*
021400040309     C*                  CALL      'TRUL06R'
021500040309     C*                  PARM                    KPJBA
021600040309     C*                  MOVEL     KPJBU         DSUL06
021700040309     C*                  MOVEA     LIN           L1
021800020402     C* VEDO SE SONO SIMFEL O REMOTO
021900020701     C                   EVAL      VidFil = SimPOU
022000020701     C                   EVAL      VidPOG = SimPOU
022100090417     c                   if        tipo = 'A' and simpou = 046
022200090417     c                   seton                                        51
022300090417     c                   end
022400000000     C                   ENDSR
022500000000      *--------------------------------------------------------------*
022600000000      * Routine per i controlli sui campi del formato video          *
022700000000      *--------------------------------------------------------------*
022800000000     C     CONTR         BEGSR
022900000000      *
023000090515     C                   SETOFF                                       90
023100020308     c                   movea     '000000'      *in(41)
023200090904      *
023300090904     c* se il campo v03pis è impostato valorizzo i campi della spedizione
023400090904     c* solo se non sono stati impostati a mano o al giro di barcode che poi
023500090904     c* ha dato l'errore
023600090904     c                   clear                   ds_pis
023700090904     c   73              if        v03pis = *blank
023800090904     c                   seton                                        1190
023900090904     C                   GOTO      ENDCTL
024000090904     c                   else
024100090904     c                   movel     v03pis        ds_pis
024200090904      *
024300090904     c                   if        vidfil = 0 and cdpa1v = 0 and vidDAL = 0
024400090904     c                   move      ds_pisfil     vidfil
024500090904     c                   move      ds_piscod     cdpa1v
024600090904     c                   move      ds_pisdata    vidDAL
024700090904      *
024800090904     c* controllo il check digits
024900090904     c                   exsr      srchkdgt
025000090904      *
025100090904     c                   if        *in10
025200090904     C                   GOTO      ENDCTL
025300090904     c                   end
025400090904      *
025500090904     c                   else
025600090904     c                   if        vidfil<>ds_pisFIL or
025700090904     c                             cdpa1v<>ds_pisCOD or vidDAL<>ds_pisdata
025800090904     c                   seton                                        1190
025900090904     C                   GOTO      ENDCTL
026000090904     c                   end
026100090904      *
026200090904     c                   end
026300090904     c                   end
026400090904      *
026500030416     c                   setoff                                       48
026600000000      * Verifico il codice filiale
026700000000     C                   MOVE      *BLANKS       DESFIL
026800000000     C     VIDFIL        IFGT      0
026900000000     C     VIDFIL        CHAIN     AZORG01L                           41
027000000000     C  N41              MOVEL     ORGDES        DESFIL
027100000000     C   41              SETON                                        90
027200000000     C   41              GOTO      ENDCTL
027300000000      *
027400000000      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO LE MIE
027500040305     C*    SIMFEL        IFNE      0
027600040305     C*    VIDFIL        LOOKUP    L1                                     99
027700040305     C* N99              SETON                                        42  90
027800040305     C* N99              GOTO      ENDCTL
027900040305     C*                  ENDIF
028000020308     c                   else
028100020308     C                   SETON                                        9041
028200020308     C                   GOTO      ENDCTL
028300000000     C                   ENDIF
028400000000      *
028500000000      * Controlli di congruenza
028600020312      * Se immesso il cd.padroncino
028700020307     C     CDPA1V        IFeq      *zero
028800020307     C                   SETON                                        9045
028900000000     C   90              GOTO      ENDCTL
029000000000     C                   ENDIF
029100000000      *
029200000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
029300020307     C     VIDDAL        ifeq      0
029400020308     C                   SETON                                        4390
029500000000     C                   GOTO      ENDCTL
029600000000     C                   END
029700000000      * Controllo data iniziale
029800000000     C     VIDDAL        IFGT      0
029900940926     C                   Z-ADD     VIDDAL        G02DAT
030000000000     C                   MOVEL     *BLANK        G02ERR
030100940926     C                   CALL      'XSRDA8'
030200000000     C                   PARM                    WLBDAT
030300000000     C     G02ERR        IFEQ      '1'
030400020308     C                   SETON                                        44  90
030500000000     C                   GOTO      ENDCTL
030600000000     C                   ENDIF
030700020307     C                   Z-ADD     G02INV        DALRIB            8 0
030800020308     C                   Z-ADD     G02dat        viddal
030900090421     c* se RAM PUÒ IMPOSTARE DATA < 01/05/2009
031000090421     C                   IF        TIPO = 'A' AND SIMPOU <> 046
031100090421     C                             AND DALRIB > 20090500
031200090421     C                   SETON                                        4490
031300090421     C                   GOTO      ENDCTL
031400020307     c                   endif
031500090421     c                   endif
031600030416      * SE coop deve essere gestito il tipo servizio
031700030416     C                   if        tipo = 'C' and vidtsr = *blank
031800030416     C                   SETON                                        4890
031900030416     C                   GOTO      ENDCTL
032000030416     C                   END
032100020402     c                   movel     vidfil        kpdr              7 0
032200020402     c                   move      cdpa1v        kpdr
032300021203     c     kapd          chain     fiapd01l
032400090929     c*****              movel     apdrsc        despad
032500090929     c                   movel     apdrsf        despad
032600020402     c                   setoff                                       47
032700131008     c                   clear                   tibs42ds
032900131008     c                   movel     vidfil        i42pge
033100131008     c                   call      'TIBS42R'
033200131008     c                   parm                    tibs42ds
033300131008     c                   movel     o42uni        uteautds
033400020402      * se abilitato con *in47 abilito la possibilità di lancio
033500030203     c                   if        tipo = 'A'
033600100421     c                   if        §autsdt <> 'S'
033700020402     c                   seton                                        4790
033800020402     c                   endif
033900030203     c                   else
034000030203     c                   if        §autcrsc <> 'S'
034100030203     c                   seton                                        4790
034200030203     c                   endif
034300030203     c                   endif
034400090421      * data ricezione
034500090417     C     VIDDrc        ifeq      0
034600090417     C                   SETON                                        5090
034700090417     C                   GOTO      ENDCTL
034800090417     C                   END
034900090421      * Controllo data ricezione copia firmata
035000090417     C     VIDDrc        IFGT      0
035100090417     C                   Z-ADD     VIDDrc        G02DAT
035200090417     C                   MOVEL     *BLANK        G02ERR
035300090417     C                   CALL      'XSRDA8'
035400090417     C                   PARM                    WLBDAT
035500090417     C     G02ERR        IFEQ      '1'
035600090417     C                   SETON                                        50  90
035700090417     C                   GOTO      ENDCTL
035800090417     C                   ENDIF
035900090417     C                   Z-ADD     G02INV        Dtadrc            8 0
036000090417     C                   Z-ADD     G02dat        viddrc
036100090417     c                   endif
036200090424     C                   if        viddrc <> 0 and dtadrc > dataoggi
036300090424     C                   SETON                                        5090
036400090424     C                   GOTO      ENDCTL
036500090424     C                   END
036600000000      *
036700090904      *
036800090904      *   Se sono arrivato qua senza errori e se richiesto
036900090904      *    L'input tramite sparatura deve confermare semplicemente
037000090904      *    con la pistola quindi è come se avesse dato F6_Conferma
037100090904     c  n90              if        combts = 'L'
037200090904     c                   eval      *inkf ='1'
037300090904     c                   end
037400090904      *
037500000000     C     ENDCTL        ENDSR
037600090904     C*-----------------------------------------------------*
037700090904     C* controllo check digits
037800090904     C*-----------------------------------------------------*
037900090904     C     srchkdgt      BEGSR
038000090904     c                   movel     ds_pis        barcode
038100090904     C                   clear                   trul28ds
038200090904     c                   eval      i28mod = 'BAR'
038300090904     c                   eval      i28cod = barcode
038400090904     c                   call      'TRUL28R1'
038500090904     c                   parm                    trul28ds
038600090904     c* se reperito barcode imposto i campi
038700090904     c                   if        o28err <> *blanks or  o28cod <> ds_pis
038800090904     c                   seton                                        1090
038900090904     c                   end
039000090904     C                   ENDSR
