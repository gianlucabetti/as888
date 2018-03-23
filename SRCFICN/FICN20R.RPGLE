000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030116      *       prompt di lancio stampa valor.giornaliera cooperative
000400000000      *****************************************************************
000500030205     Ftntbe01L  IF   E           K DISK
000600030116     Ffiapd01L  IF   E           K DISK
000700030116     Fficn20D   CF   E             WORKSTN
000800030116      *****************************************************************
001400030205     D dccg          E DS
001500030116     d KPJBA         E DS
001600030120     D Tibs36ds      E DS
001700000000     D CNCR80        E DS
001800000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900030116     D fnlv24ds      E DS
002000030212     D ficn20ds      e DS                  inz
002001131007     D tibs42ds      e DS                  inz
002002131007     D uteautds      e DS
002100030212     d com03           s              3
002200940926     D*
002300000000     D WLBDAT          DS
002400940926     D  G02DAT                 1      8  0
002500940926     D  G02INV                 9     16  0
002600940926     D  G02ERR                17     17
002700940926     D  G02TGI                18     22  0
002800030205      *
002900030205     D WGIDAT          DS
003000030205     D  GIODAT                 1      8  0
003100030205     D  GIOINV                 9     16  0
003200030205     D  GIOTGI                17     21  0
003300030205      *
003400000000      *
003500030116     D digits          C                   CONST('0123456789')
003600030116      *****************************************************************
003700000000     C     *ENTRY        PLIST
003800000000     C                   PARM                    KPJBA
003900030217     c                   movel     kpjbu         tipo              1
004000030217     c* se chiamato dall'azione "Stampa specifica per COOP"
004100030217     c* non da la possibilità di specificare opzioni ad uso interno
004200030217     c                   if        tipo = 'C'
004300030217     c                   seton                                        60
004400030217     c                   end
004500030205     c*
004600030205     C     KTbe          KLIST
004700030205     C                   KFLD                    tbecod
004800030205     C                   KFLD                    tbeke1
004900030116     C*
005000030116     C     Kapd          KLIST
005100030116     C                   KFLD                    D1ctip
005200030116     C                   KFLD                    kpdr              7 0
005300030116     C                   move      'C'           D1ctip            1
005400030120      *
005500030120      *  In base all'utente prende il P.O.
005600030120     C                   CLEAR                   Tibs36Ds
005700030120     c                   EVAL      I36ute = Knmus
005800030120     c                   EVAL      I36Tla = 'L'
005900030120     C                   CALL      'TIBS36R'
006000030120     C                   PARM                    Tibs36Ds
006100030123      * se utente di sede
006200030212     c*?                 eval      utesed ='N'
006300030212     c                   if        o36pou = 046
006400030212     c                   seton                                        77
006500030212     c*?                 eval      utesed ='S'
006600030212     c                   end
006700000000      *--------------------------------------------------------------*
006800000000      *             M A I N      L I N E
006900000000      *--------------------------------------------------------------*
007000000000     C                   Z-ADD     1             CODUT
007100000000     C                   CALL      'X§PARUT'
007200000000     C                   PARM                    UT§DSE
007300000000     C                   MOVEL     RAGUT         RSUT             20
007400000000     C                   MOVEL     RAGUT         RSUT2            20
007500000000     C                   MOVEL     REC80         CNCR80
007600030116      *
007700030116     C     INIZIO        tag
007800030117     C                   Z-ADD     *ZERO         D1DCD1                         LNPcoop
007900030120     C                   Z-ADD     *ZERO         d1dca1                         LNPcoop
008000030116      *
008100030116     C                   EXSR      CARTAB
008200030116      *                    -----------
008300000000      * Inizializzo i campi a video
008400030120     C                   move      *ZERO         d1dcd2                         Codcoop
008500030117     C                   move      *ZERO         D1DCA2                         Codcoop
008600030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
008700030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
008800030303     C*                  move      data_Lim_Coop D1ddal
008900030303     C*                  move      data_Lim_Coop D1dal
009000030217     C* Per stampa ad uso esterno i valori devono essere
009100030217     c* dettaglio=S, riepilogo=N, uso interno=N, ore=S
009200030217     c                   if        tipo = 'C'
009300030217     c                   move      'N'           D1CESN
009400030217     c                   move      'S'           D1C0SN
009500030217     c                   move      'S'           D1Dett
009600030217     c                   move      'N'           D1RIE
009700030217     c                   else
009800030116     c                   move      'N'           D1CESN
009900030121     c                   move      'S'           D1C0SN
010000030121     c                   move      'S'           D1Dett
010100030129     c                   move      'N'           D1RIE
010200030217     c                   end
010300030120      *
010400030120     C                   if        o36pou = 046
010500030120     C                   Z-ADD     *ZERO         D1DCD1                         LNPcoop
010600030120     C                   move      *zero         d1dcd2                         Codcoop
010700030120     C                   Z-ADD     *All'9'       d1dca1                         LNPcoop
010800030120     C                   End
010900030120     C                   move      *All'9'       D1DCA2                         Codcoop
011000000000      * Emetto il formato video
011100030116     C     Video1        TAG
011200020308     C                   SETOFF                                       90
011300030116     C                   EXFMT     CN20D01
011400000000      *
011500000000     C   KC              GOTO      FINE                                         F3=USCITA
011600000000      *
011700000000      * Eseguo i  controlli sui campi immessi a video
011800000000     C                   EXSR      CONTR
011900000000      *
012000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
012100020308     C  N90
012200020308     CanNkf              SETON                                          90      F6=CONFERMA
012300000000      *
012400000000      * Per errore generico riemetto il formato video
012500030116     C   90              GOTO      Video1
012600000000      *
012700020307      * Richiamo programma di stampa
012800020308     c                   if        *inkf
012900030212     c                   clear                   ficn20ds
013000030116      * compone il codice numerico
013100030212     c     D1DCD1        mult      10000         f20cda
013200030212     c                   move      d1dcd2        f20cda
013300030212     c     d1dca1        mult      10000         f20ca
013400030212     c                   move      D1DCA2        f20ca
013500030116      * imposta le date
013600030212     c                   z-add     DalAmg        f20dti
013700030212     c                   z-add     AlAmg         f20dtf
013800030116      * imposta i flags
013900030212     c                   move      d1cesn        f20ore
014000030212     c                   move      d1c0sn        f20com
014100030212     c                   move      d1dett        f20det
014200030212     c                   move      d1rie         f20rie
014300030212     c                   move      'L'           f20ela
014400030116      *
014500030212     c                   movel(p)  ficn20ds      kpjbu
014600030116     c                   move      'FN22'        kcoaz
014700030116      * Lancio Batch
014800030124     c                   if        knmus = *all'1' or
014900030124     c                             knmus = 'EDPAB'
015000030124     c                   call      'FICN22R'
015100030124     c                   parm                    kpjba
015200030124     c                   else
015300020626     c                   call      'BCH10'
015400020624     c                   parm                    kpjba
015500030124     c                   end
015600020308     c                   endif
015700000000      *
015800080929      * Si deve uscire dal programma
015900080929     C********           GOTO      INIZIO
016000030116      *
016100000000     C     FINE          TAG
016200000000     C                   SETON                                        LR
016300000000      *--------------------------------------------------------------*
016400000000      * Routine per i controlli sui campi del formato video          *
016500000000      *--------------------------------------------------------------*
016600000000     C     CONTR         BEGSR
016700000000      *
016800030116     c                   movea     '0000000000'  *in(41)
016900030116     c                   clear                   d1ccd1
017000030116     c                   clear                   d1ccd2
017100030116      * --------------
017200030120      * Se non è un profilo di sede esegue i controlli sui codici
017300030120     C                   if        O36Pou <> 046
017400030120      *
017500030116      * FILIALE OBBLIGATORIA
017600030117     C     D1DCD1        IFEQ      0
017700030116     C                   SETON                                        4690
017800030116     C                   GOTO      ENDCT1
017900030116     C                   ENDIF
018000030116      *
019100030116      *
019200030120     C     d1dca1        IFEQ      0
019300030120     C     d1dca1        orlt      d1dcd1
019400030120     C                   move      D1DCD1        d1dca1
019500030116     C                   ENDIF
020600030116      *
020700030120     C                   Endif
020800030120      *
020801130808      *controllo se utente autorizzato alla funzione
020802131007     c                   clear                   tibs42ds
020803131007     c                   movel     d1dcd1        i42pge
020805131007     c                   call      'TIBS42R'
020806131007     c                   parm                    TIBS42ds
020807131007     c                   movel     o42uni        uteautds
020808130808     c                   if        §autretc <> 'S'
020809130808     C                   SETON                                        4690
020810130808     c                   goto      endct1
020811130808     c                   endif
020900030116      * --------------
021000030116      *
021100030116      * RICERCA
021200030120     C     '?'           SCAN      d1dcd2                                 88
021300030116     C     *IN88         IFEQ      *ON
021400030116     C                   MOVEL     *BLANKS       d24rsc
021500030116     C                   Z-ADD     0             d24pdr
021600030117     C                   Z-ADD     D1DCD1        d24FIL
021700030116     C                   MOVEL     'R'           d24FLG
021800030116     C                   MOVEL     'C'           d24tip
021900030124     C                   MOVEL(p)  fnlv24ds      KPJBU
022000030116     C                   CALL      'FNLV24R'
022100030116     C                   PARM                    KPJBA
022200030116     C                   MOVEL     KPJBU         fnlv24ds
022300040105     C                   SETON                                          90
022400030116      *
022500030116      * CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
022600030116     C     d24pdr        IFNE      0
022700030117     C                   MOVEL     d24pdr        D1DCD1
022800030120     C                   MOVEL     *ZEROS        d1dcd2
022900030120     C                   MOVE      d24pdr        d1dcd2
023000030116     C                   MOVEL     d24rsc        d1ccd1
023100030116     C                   ELSE
023200030116     C                   MOVE      *BLANKS       d1ccd1
023300030117     C                   MOVE      *zeros        D1DCD1
023400030120     C                   MOVE      *zeros        d1dcd2
023500030116     C                   SETON                                        4790
023600030116     C                   ENDIF
023700030116      *
023800030116     C                   GOTO      ENDCT1
023900030116     C                   ENDIF
024000030116      * --------------
024100030116      * RICERCA
024200030117     C     '?'           SCAN      D1DCA2                                 88
024300030116     C     *IN88         IFEQ      *ON
024400030116     C                   MOVEL     *BLANKS       d24rsc
024500030116     C                   Z-ADD     0             d24pdr
024600030120     C                   Z-ADD     d1dca1        d24FIL
024700030116     C                   MOVEL     'R'           d24FLG
024800030116     C                   MOVEL     'C'           d24tip
024900030116     C                   MOVEL(p)  fnlv24ds      KPJBU
025000030116     C                   CALL      'FNLV24R'
025100030116     C                   PARM                    KPJBA
025200030116     C                   MOVEL     KPJBU         fnlv24ds
025300040105     C                   SETON                                          90
025400030116      *
025500030116      * CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
025600030116     C     d24pdr        IFNE      0
025700030120     C                   MOVEL     d24pdr        d1dca1
025800030117     C                   MOVEL     *ZEROS        D1DCA2
025900030117     C                   MOVE      d24pdr        D1DCA2
026000030116     C                   MOVEL     d24rsc        d1ccd2
026100030116     C                   ELSE
026200030116     C                   MOVE      *BLANKS       d1ccd2
026300030120     C                   MOVE      *zeros        d1dca1
026400030117     C                   MOVE      *zeros        D1DCA2
026500030116     C                   SETON                                        4890
026600030116     C                   ENDIF
026700030116      *
026800030116     C                   GOTO      ENDCT1
026900030116     C                   ENDIF
027000000000      *
027100030116      * --------------
027200030116      *
027300030116      * Controllo range Coop
027400030120     C     Digits        Check     d1dcd2:1
027500030116     C                   If        %Found
027600030116     C                   SETON                                        4790
027700030116     C                   GOTO      endct1
027800030116     c                   end
027900030116      *
028000030120     C     d1dcd2        IFne      '0000'
028100030120     C     d1dcd2        andne     *blank
028200030117     C     D1DCA2        ifeq      '0000'
028300030117     C     D1DCA2        oreq      *blank
028400030120     C                   move      d1dcd2        D1DCA2
028500030116     C                   ENDIF
028600030116     C                   ENDIF
028700030116      *
028800030117     C     Digits        Check     D1DCA2:1
028900030116     C                   If        %Found
029000030116     C                   SETON                                        4890
029100030116     C                   GOTO      endct1
029200030116     c                   end
029300030116      *
029400030117     c     D1DCD1        mult      10000         fld701            7 0
029500030120     c                   move      d1dcd2        fld701
029600030116      *
029700030120     c     d1dca1        mult      10000         fld702            7 0
029800030117     c                   move      D1DCA2        fld702
029900030116      *
030000030116     C     fld702        iflt      fld701
030100030116     C                   SETON                                        4990
030200030116     C                   GOTO      endct1
030300030116     C                   END
030400030116      * --------------
030500030116      *
030600030116      * Decodifiche Cooperative
030700030120     c                   if        o36pou <> 046
030800030120     c                   if        D1DCD2 <> '0000'
030900030116     C                   move      fld701        kpdr              7 0
031000030116     C     Kapd          chain     fiapd01l
031100030116     c                   if        not %Found(fiapd01l)
031200030116     C                   SETON                                        4790
031300030116     C                   GOTO      endct1
031400030116     C                   Else
031500100223     C                   MOVEL     apdrsf        d1ccd1
031600030116     C                   End
031700030120     C                   EndIf
031800030120     C                   EndIf
031900030116      *
032000030117     c                   if        D1DCA2 <> '9999'
032100030116     C                   move      fld702        kpdr
032200030116     C     Kapd          chain     fiapd01l
032300030116     c                   if        not %Found(fiapd01l)
032400030116     C                   SETON                                        4890
032500030116     C                   GOTO      endct1
032600030116     C                   Else
032700100223     C                   MOVEL     apdrsf        d1ccd2
032800030116     C                   End
032900030116     C                   EndIf
033000030116      * --------------
033100030116      *
033200020624      * controllo immissione delle date
033300030116     C     d1DDAL        ifeq      0
033400020308     C                   SETON                                        4390
033500030116     C                   GOTO      endct1
033600030116     C                   Else
033700030116     C                   Z-ADD     d1DDAL        G02DAT
033800000000     C                   MOVEL     *BLANK        G02ERR
033900940926     C                   CALL      'XSRDA8'
034000000000     C                   PARM                    WLBDAT
034100000000     C     G02ERR        IFEQ      '1'
034200020308     C                   SETON                                        44  90
034300030116     C                   GOTO      endct1
034400000000     C                   ENDIF
034500030116     C                   Z-ADD     G02INV        DalAmg            8 0
034600030116     C                   Z-ADD     G02dat        D1ddal
034700020307     c                   endif
034800020624      * Controllo data finale
034900030116     C     d1Dal         ifeq      0
035000030127     C****               SETON                                        4190
035100030127     C****               GOTO      endct1
035200030127     C                   Z-ADD     d1DDal        d1Dal
035300030127     C                   End
035400030116     C                   Z-ADD     d1Dal         G02DAT
035500020624     C                   MOVEL     *BLANK        G02ERR
035600020624     C                   CALL      'XSRDA8'
035700020624     C                   PARM                    WLBDAT
035800020624     C     G02ERR        IFEQ      '1'
035900020624     C                   SETON                                        42  90
036000030116     C                   GOTO      endct1
036100020624     C                   ENDIF
036200030116     C                   Z-ADD     G02INV        AlAmG             8 0
036300030116     C                   Z-ADD     G02dat        d1dal
036400030127     c*
036500030116     C     DalAmg        ifgt      AlAmg
036600020624     C                   SETON                                        4590
036700030116     C                   GOTO      endct1
036800020624     C                   END
036900000000      *
037000030129      *  se richiesto il riepilogo la stampa non deve essere di dettaglio
037100030129      *   giornaliero
037200030129     c                   if        d1rie='S'and d1dett ='S'
037300030129     C                   SETON                                        5190
037400030129     C                   GOTO      endct1
037500030129     C                   END
037600030129      *
037700030129      * Non si può fare il riepilogo di un solo giorno
037800030129     c                   if        d1rie='S'and d1ddal = d1dal
037900030129     C                   SETON                                        5290
038000030129     C                   GOTO      endct1
038100030129     C                   END
038200030129      *
038300030116     C     endct1        ENDSR
038400030116      *--------------------------------------------------------------*
038500030116      * CARTAB - SUBROUTINE CARICAMENTO TABELLE                      *
038600030116      *--------------------------------------------------------------*
038700030116     C     CARTAB        BEGSR
038800030116      *
038900110207     C                   MOVEL     o36pou        D1DCD1
039000110207     C                   MOVEL     o36pou        d1dca1
040300030205      *
040400030205      *  Reperisce il nr. dei giorni oltre i quali non è possibile
040500030205      *  inserire rettifiche su conteggi Cooperative confermati.
040600030205     C                   movel(P)  '1'           tbeke1
040700030205     c                   movel     'CCG'         tbecod
040800030205     C     Ktbe          chain     tntbe01l                           21
040900030205     C     *in21         ifeq      *on
041000030205     C     tbeatb        orne      *blanks
041100030205     c                   clear                   dccg
041200030205     c                   else
041300030205     c                   movel     tbeuni        dccg
041400030205     c                   end
041500030205     C*
041600030205     C* Imposta la UDATE  da TIME
041700030205     C                   TIME                    W0140            14 0
041800030205     C                   MOVE      W0140         UDATE8            8 0
041900030205     C*
042000030205     C                   MOVE      UDATE8        G02DAT
042100030205     C                   MOVEL     *BLANK        G02ERR
042200030205     C                   CALL      'XSRDA8'
042300030205     C                   PARM                    WLBDAT
042400030205     C* nr.assoluto di oggi
042500030205     c                   z-add     G02Tgi        oggi_num          5 0
042600030205     C*
042700030205     C     G02TGI        SUB       §cgGUV        GIOTGI
042800030205     C                   CALL      'XSRGI8'
042900030205     C                   PARM                    WGIDAT
043000030205     C* Data limite Cooperative pre modificare o aggiungere rettifiche
043100030205     c                   Z-ADD     GIODAT        data_Lim_Coop     8 0
043200030205     C*
043300030205     C*
043400030116     C                   ENDSR
043500030116      *--------------------------------------------------------------*
