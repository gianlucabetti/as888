000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Confronto fatture
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700040823     Ffieu84f   iF   E           k disk
000800040819     Ffieu84D   CF   E             WORKSTN
000900030116      *****************************************************************
001000030116     d KPJBA         E DS
001100030120     D Tibs36ds      E DS
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400130822     D OG143         E DS
001500040819     D fieu84ds      e DS                  inz
001600040901     d  xgrp                  75    104    dim(10) descend
001700940926     D*
001800000000     D WLBDAT          DS
001900940926     D  G02DAT                 1      8  0
002000940926     D  G02INV                 9     16  0
002100940926     D  G02ERR                17     17
002200940926     D  G02TGI                18     22  0
002300030205      *
002400030116     D digits          C                   CONST('0123456789')
002500071022     D int_batch       C                   CONST('Interrattiva o Batch [I/B]')
002600030116      *****************************************************************
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900030120      *
003000030825     C* Imposta la UDATE  da TIME
003100030825     C                   TIME                    W0140            14 0
003200030825     C                   MOVE      W0140         UDATE8            8 0
003300030825     C*
003400030120      *  In base all'utente prende il P.O.
003500030120     C                   CLEAR                   Tibs36Ds
003600030120     c                   EVAL      I36ute = Knmus
003700030120     c                   EVAL      I36Tla = 'L'
003800030120     C                   CALL      'TIBS36R'
003900030120     C                   PARM                    Tibs36Ds
004000030827     c                   movel     KNMUS         ALFA3             3
004100030829     c                   setoff                                       0102
004200030829      * solo per EDP....:
004300030829     c                   if        ALFA3 ='EDP'
004400030829     c                   seton                                        01
004500030829     c                   end
004600030829      * per utenti di filiale:
004700030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004800030829     c                   seton                                        02
004900030829     c                   end
005000000000      *--------------------------------------------------------------*
005100000000      *             M A I N      L I N E
005200000000      *--------------------------------------------------------------*
005300000000     C                   Z-ADD     1             CODUT
005400000000     C                   CALL      'X§PARUT'
005500000000     C                   PARM                    UT§DSE
005600000000     C                   MOVEL     RAGUT         RSUT             20
005700000000     C                   MOVEL     RAGUT         RSUT2            20
005800000000     C                   MOVEL     REC80         CNCR80
005900040819      *
006000040819     C     INIZIO        tag
006100030825      *
006200040819     c                   clear                   D1NTW
006300030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006400030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006500041007     C                   move      'F'           D1Dsi
006600040823     c                   clear                   D1Ifd
006700040823     c                   clear                   D1Iad
006800040823     c                   clear                   D1Efd
006900040823     c                   clear                   D1Ead
007000041008     c                   movel     'F/S  '       D1sper
007100041008     c   01              movel     'F/S/I'       D1sper
007200040823     c                   move      'S'           D1Ifsn                         S/N
007300040823     c                   move      'S'           D1Iasn                         S/N
007400040823     c                   move      'S'           D1Efsn                         S/N
007500040823     c                   move      'S'           D1Easn                         S/N
007600040831     C                   move      'N'           D1DDet                         S/N
007700041008     C                   move      'F'           D1Tot
007800040827     C                   move      'S'           D1stna                         S/N
007900040827     C                   move      'S'           D1stni                         S/N
008000041011     C                   move      *all'9'       D1stde                         00
008100040820     C                   move      'N'           D1cfpc                         S/N
008200150203     C                   move      'S'           D1rie                          S/N
008300040827     C                   clear                   eu84w01
008400040831     C                   move      'I'           w1grie
008500040831     C                   move      '001'         w1grp1
008600030915      *
008700000000      * Emetto il formato video
008800030116     C     Video1        TAG
008900020308     C                   SETOFF                                       90
009000040819     C                   EXFMT     eu84d01
009100040827      *
009200040827      * Gruppi da escludere
009300040827     C   KH              exsr      aggtab
009400000000      *
009500000000     C   KC              GOTO      FINE                                         F3=USCITA
009600000000      *
009700000000      * Eseguo i  controlli sui campi immessi a video
009800000000     C                   EXSR      CONTR
009900000000      *
010000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
010100030915     C   90
010200040819     CorNkf              GOTO      Video1
010300040831      *
010400040831     C     Video2        TAG
010500040831     C                   SETOFF                                       90
010600040831      *
010700040831     C                   EXFMT     eu84w01
010800040831     C   KC              GOTO      FINE                                         F3=USCITA
010900040831     C   KL              GOTO      Video1                                       F3=USCITA
011000040831      *
011100040831      * Se non vi sono errori ma non e' premuto F6 emetto video
011200040831     C   90
011300040831     CorNkf              GOTO      Video2
011400040831      *
011500040820     c                   clear                   fieu84ds
011600040831      * compone il codice numerico
011700040820     c                   move      d1ntw         e84ntw
011800040901     c                   select
011900040901     c                   when      D1Dsi = 'I'
012000040901     c                   z-add     DalAmg        e84diI
012100040901     c                   z-add     AlAmg         e84dfI
012200040901     c                   when      D1Dsi = 'S'
012300040901     c                   z-add     DalAmg        e84diS
012400040901     c                   z-add     AlAmg         e84dfS
012500040901     c                   when      D1Dsi = 'F'
012600040901     c                   z-add     DalAmg        e84diF
012700040901     c                   z-add     AlAmg         e84dfF
012800040901     c                   endsl
012900040823     c                   move      D1Ifsn        e84If
013000040820     c                   move      D1Ifc         e84Ifc
013100040820     c                   move      D1Ifr         e84Ifr
013200040823     c                   move      D1Iasn        e84Ia
013300040820     c                   move      D1Iac         e84Iac
013400040820     c                   move      D1Iar         e84Iar
013500040823     c                   move      D1Efsn        e84Ef
013600040820     c                   move      D1Efc         e84Efc
013700040820     c                   move      D1Efr         e84Efr
013800040823     c                   move      D1Easn        e84Ea
013900040820     c                   move      D1Eac         e84Eac
014000040831     C                   move      D1Ear         e84Ear
014100040831     C                   move      D1dDet        e84Det                        S/N
014200040820     C                   move      D1Totf        e84Tof                        S/N
014300040820     C                   move      D1totr        e84tor                        S/N
014400040820     C                   move      D1tots        e84tos                        S/N
014500040825     C                   move      D1totp        e84top                        S/N
014600040820     C                   move      D1stna        e84sna                        S/N
014700040820     C                   move      D1stni        e84sni                        S/N
014800040820     C                   move      D1stde        e84sde                        S/N
014900040820     C                   move      D1cfpc        e84fpc                        S/N
015000150203     C                   move      D1rie         e84rie                        S/N
015100040827     C                   move      w1grie        e84gie
015200040827     C                   move      w1grp1        e84gr1
015300040827     C                   move      w1grp2        e84gr2
015400040827     C                   move      w1grp3        e84gr3
015500040827     C                   move      w1grp4        e84gr4
015600040827     C                   move      w1grp5        e84gr5
015700040827     C                   move      w1grp6        e84gr6
015800040827     C                   move      w1grp7        e84gr7
015900040827     C                   move      w1grp8        e84gr8
016000040827     C                   move      w1grp9        e84gr9
016100040827     C                   move      w1grp0        e84gr0
016200040827     c                   sorta     xgrp
016300040819      *
016400071022     c                   if        knmus = 'EDPAB'
016500071022     c     int_batch     DSPLY                   risposta          1
016600071022     c                   end
016700071022      *
016800040819      *  richiama il pgm che esegue il consolidamento
016900040819     c                   movel     fieu84ds      kpjbu
017000040826     c                   movel     'EU85'        kcoaz
017100040826     c     kcoaz         chain     kfazn11l
017200040826     c                   if        not %Found(kfazn11l)
017300071022     c                             or risposta = 'I'
017400071022     c                             or risposta = 'i'
017500040826     c                   call      'FIEU85R'
017600040826     c                   parm                    kpjba
017700040826     c                   else
017800041103     c                   eval      KJOST ='H'
017900041103     c                   if        ALFA3 ='EDP'
018000040819     c                   call      'BCH09'
018100040819     c                   parm                    kpjba
018200041103     c                   end
018300040819     c                   call      'BCH10'
018400040819     c                   parm                    kpjba
018500040826     c                   end
018600000000      *
018700041007     C                   GOTO      INIZIO
018800041007     C******             GOTO      Video1
018900041007      *
019000000000     C     FINE          TAG
019100000000     C                   SETON                                        LR
019200040826      *--------------------------------------------------------------*
019300040826      * Aggiorna tabella confronti
019400040826      *--------------------------------------------------------------*
019500040826     C     AGGtab        BEGSR
019600040826     c*
019700040826     c                   clear                   kpjbu
019800040826     c                   call      'FIEU84R2'
019900040826     c                   parm                    kpjba
020000040826     c*
020100040826     C                   ENDSR
020200040827      *--------------------------------------------------------------*
020300040827      * Routine per i controlli sui campi del formato video          *
020400040827      *--------------------------------------------------------------*
020500040827     C     CONTR         BEGSR
020600040827      *
020700030116     c                   movea     '0000000000'  *in(41)
020800040819     c                   movea     '0000000000'  *in(51)
020900040819     C     d1ntw         ifeq      *blank
021000040819     C                   SETON                                        4190
021100040819     C                   GOTO      endct1
021200040823     C                   Else
021300130822      ***
021400130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
021500130822      * controlla la LNP se network EDI estero
021600130822     c                   z-add     0             dove              3 0
021700130822     c     digits        check     d1ntw         dove
021800130822     c                   if        dove > 0
021900130822      * ci sono alfanumerici
022000130822     C                   SETON                                        4190
022100130822     C                   GOTO      endct1
022200130822     C                   Else
022300130822     c                   move      d1ntw         ntw_num3          3 0
022400130822     c     ntw_num3      chain     azorg01l
022500130822     c                   if        not %Found(azorg01l)
022600130822     C                   SETON                                        4190
022700130822     C                   GOTO      endct1
022800130822     C                   Else
022900130822     c                   eval      og143 = orgde3
023000130822     c                   if        §ogNTW <> 'EEX'
023100130822     C                   SETON                                        4190
023200130822     C                   GOTO      endct1
023300130822     c                   end
023400130822     c                   end
023500130822     C                   End
023600130822      *
023700130822     C                   End
023800130822      ***
023900040823     C     d1ntw         chain     fieu84f
024000040823     c                   if        %Found(fieu84f)
024100040823     c                   if        t84ifc = 'X'
024200040823     c                   move      t84ifc        d1ifc             1
024300040823     c                   clear                   d1ifd
024400040825     c                   movel     'Con'         d1ifd
024500040825     c                   move      'Competenze'  d1ifd
024600040823     c                   end
024700040823     c                   if        t84ifr = 'X'
024800040823     c                   move      t84ifr        d1ifr             1
024900040823     c                   clear                   d1ifd
025000040825     c                   movel     'Con'         d1ifd
025100040823     c                   move      'Ricavi'      d1ifd
025200040823     c                   end
025300040823     c                   if        t84iac = 'X'
025400040823     c                   move      t84iac        d1iac             1
025500040823     c                   clear                   d1iad
025600040825     c                   movel     'Con'         d1iad
025700040823     c                   move      'Competenze'  d1iad
025800040823     c                   end
025900040823     c                   if        t84iar = 'X'
026000040823     c                   move      t84iar        d1iar             1
026100040823     c                   clear                   d1iad
026200040825     c                   movel     'Con'         d1iad
026300040823     c                   move      'Ricavi'      d1iad
026400040823     c                   end
026500040823     c                   if        t84efc = 'X'
026600040823     c                   move      t84efc        d1efc             1
026700040823     c                   clear                   d1efd
026800040825     c                   movel     'Con'         d1efd
026900040823     c                   move      'Competenze'  d1efd
027000040823     c                   end
027100040823     c                   if        t84efr = 'X'
027200040823     c                   move      t84efr        d1efr             1
027300040823     c                   clear                   d1efd
027400040825     c                   movel     'Con'         d1efd
027500040823     c                   move      'Ricavi'      d1efd
027600040823     c                   end
027700040823     c                   if        t84eac = 'X'
027800040823     c                   move      t84eac        d1eac             1
027900040823     c                   clear                   d1ead
028000040825     c                   movel     'Con'         d1ead
028100040823     c                   move      'Competenze'  d1ead
028200040823     c                   end
028300040823     c                   if        t84ear = 'X'
028400040823     c                   move      t84ear        d1ear             1
028500040823     c                   clear                   d1ead
028600040825     c                   movel     'Con'         d1ead
028700040823     c                   move      'Ricavi'      d1ead
028800040823     c                   end
028900040823     C                   End
029000040823     C                   End
029100030116      * --------------
029200020624      * controllo immissione delle date
029300030116     C                   Z-ADD     d1DDAL        G02DAT
029400000000     C                   MOVEL     *BLANK        G02ERR
029500940926     C                   CALL      'XSRDA8'
029600000000     C                   PARM                    WLBDAT
029700000000     C     G02ERR        IFEQ      '1'
029800020308     C                   SETON                                        44  90
029900030116     C                   GOTO      endct1
030000000000     C                   ENDIF
030100030116     C                   Z-ADD     G02INV        DalAmg            8 0
030200030116     C                   Z-ADD     G02dat        D1ddal
030300030825      *
030400020624      * Controllo data finale
030500030116     C     d1Dal         ifeq      0
030600030127     C                   Z-ADD     d1DDal        d1Dal
030700030127     C                   End
030800030825      *
030900030116     C                   Z-ADD     d1Dal         G02DAT
031000020624     C                   MOVEL     *BLANK        G02ERR
031100020624     C                   CALL      'XSRDA8'
031200020624     C                   PARM                    WLBDAT
031300020624     C     G02ERR        IFEQ      '1'
031400020624     C                   SETON                                        42  90
031500030116     C                   GOTO      endct1
031600020624     C                   ENDIF
031700030825     c*
031800030116     C                   Z-ADD     G02INV        AlAmG             8 0
031900030116     C                   Z-ADD     G02dat        d1dal
032000030127     c*
032100030116     C     DalAmg        ifgt      AlAmg
032200020624     C                   SETON                                        4590
032300030116     C                   GOTO      endct1
032400020624     C                   END
032500030129      *
032600040819      * --------------
032700040823     c*
032800040823     C                   if        d1IFc <> *blank and d1IFr <> *blank
032900040823     C                   SETON                                        5690
033000040823     C                   GOTO      endct1
033100040823     C                   END
033200040823     c*
033300040823     C                   if        d1IAc <> *blank and d1IAr <> *blank
033400040823     C                   SETON                                        5790
033500040823     C                   GOTO      endct1
033600040823     C                   END
033700040823      *
033800040823     C                   if        d1EFc <> *blank and d1EFr <> *blank
033900040823     C                   SETON                                        5890
034000040823     C                   GOTO      endct1
034100040823     C                   END
034200040823     c*
034300040823     C                   if        d1EAc <> *blank and d1EAr <> *blank
034400040823     C                   SETON                                        5990
034500040823     C                   GOTO      endct1
034600040823     C                   END
034700040825      *  totali
034800041011     C*****              if        d1tot = 'T'
034900041011     C*****              if        d1stde <> 99
035000041011     C*****              SETON                                        4790
035100041011     C*****              GOTO      endct1
035200041011     C*****              END
035300041011     C*****              END
035400041011     c*****
035500041008     c                   move      'N'           d1totf            1
035600041008     c                   move      'N'           d1totr            1
035700041008     c                   move      'N'           d1tots            1
035800041008     c                   move      'N'           d1totp            1
035900041008     c                   select
036000041008     c                   when      d1tot = 'F'
036100041008     c                   move      'S'           d1totf
036200041008     c                   when      d1tot = 'T'
036300041008     c                   move      'S'           d1totr
036400041008     c                   when      d1tot = 'S'
036500041008     c                   move      'S'           d1tots
036600041008     c                   when      d1tot = 'R'
036700041008     c                   move      'S'           d1totp
036800041008     c                   endsl
036900040903     c*
037000030116     C     endct1        ENDSR
037100030116      *--------------------------------------------------------------*
