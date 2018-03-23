000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200060904      * FNLRL2R *----------------------------------------------------*
000300060904      *         - GENERA ARCHIVIO BOLLE PER TELLUS - DOWNLOAD
000400940915      *--------------------------------------------------------------*
000500060904     FFNLRL2D   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700020805      *
000800020805      * DEFINIZIONE SCHIERE
000900960302     D*
001000080204     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001100060904     d* DS ricevuta: linea di arrivo da elaborare
001200060904     d FNLRL1DS        ds
001300060904     d  v1cpo                         3s 0
001400070406     d  v1cdat                        8s 0
001500060905     d  v1cdis                        1
001600070207     d  v1ctst                        1
001700070529     d  v1cgiro                       1a
001800070612     d  LRL1contrBL                   1a   inz
001900070612     d  LRL1botlls                    2s 0 inz
002000070529     d* Dati di output
002100070529     d* "S"--> numero minimo bolle da elaborare non raggiunto
002200070612     d  LRL1Ominimo                   1a   inz
002300070612     d  LRL1Onumbolle                 3s 0 inz
002400060904     D
002500040917     D KPJBA         E DS
002600040917     D CNCR80        E DS
002700060904     D og148         E DS
002800070529     D og146         E DS
002900040917     D UT§DSE0F      E DS
003000070124     D fnlv50ds      E DS
003100080204     D fnlv55ds      E DS
003200070529     D tibs02ds      E DS
003300101119     D DVPOtellus    E DS
003400070406     D*
003500070406     D In_Fil          s              3s 0
003600070406     D In_Dat          s              8s 0
003700070406     D Out_KeyVer      s              8
003800070406     D Out_Esito       s              1
003900070406     D Dataiso         s               d   datfmt(*iso)
004000070406     D Dataeur         s               d   datfmt(*eur)
004100050103     C**********************************************************************
004200040917     C     *ENTRY        PLIST
004300040917     C                   PARM                    KPJBA
004400070406     C*
004500070406     c* Data di elaborazione
004600070406     c                   time                    w0140            14 0
004700070406     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
004800070406     c     *eur          movel     w0080         dataeur
004900070406     c                   movel     dataeur       dataiso
005000070406     c     *iso          movel     dataiso       datcor            8 0
005100070406     c                   z-add     datcor        v1cdat
005200050103     c*
005300040917     C                   Z-ADD     1             CODUT
005400040917     C                   CALL      'X§PARUT'
005500040917     C                   PARM                    UT§DSE0F
005600040917     C                   MOVEL     REC80         CNCR80
005700040917     C                   MOVEL     RAGUT         VIDRSU
005800070124     c
005900070529     c* Se utente no EDP nascondo possibilità inclusione bolle già in distinta
006000070529     c                   setoff                                       030102
006100070529     c                   setoff                                       0405
006200070125     c* Per EDP posso lanciare qualsiasi p.o.
006300070124     c                   if        %subst(knmus:1:3)='EDP'
006400070529     c                   seton                                        0104
006500070529     c                   seton                                        0305
006600070124     c                   endif
006700070529     c* Un secondo livello può lanciare solo se stesso
006800070124     c                   if        simtpp='2'
006900070124     c                   seton                                        02
007000070124     c                   endif
007100101119     c* Aggancio tabella per max numero bolle per invio
007200101119     C                   CLEAR                   tibs02ds
007300101119     c                   clear                   dvpotellus
007400101119     C                   MOVEL     'C'           T02MOD
007500101119     C                   MOVEL     KNSIF         T02SIF
007600101119     C                   MOVEL     'VPO'         T02COD
007700101119     c                   movel(P)  'TELLUS'      t02ke1
007800101119     C                   CALL      'TIBS02R'
007900101119     C                   PARM                    KPJBA
008000101119     C                   PARM                    tibs02ds
008100101119    2C                   IF        T02ERR = *BLANKS
008200101119     C                   MOVEL     T02uni        dvpotellus
008300101119     C                   MOVEL     §vpobomax     bollemax          5 0
008400101119     c                   endif
008500070529     c
008600060904     c* Imposto lan dal p.o. utente solo se abilitato dall'organigramma
008700060905     C                   CLEAR                   V1Cdis
008800070529     C                   CLEAR                   V1Cgiro
008900060905     C                   CLEAR                   V1CPO
009000041220     C                   CLEAR                   V1DPO
009100060904     C                   EVAL      V1CPO=SIMPOU
009200060912     C     V1CPO         CHAIN     AZORG01L
009300060912     C                   IF        %FOUND(AZORG01L)
009400070124     C                   MOVEL     orgdes        v1dpo
009500070430     C                   MOVEL     ORGDE6        OG146
009600070529     C                   MOVEL     ORGDE8        OG148
009700070529     c                   else
009800070529     c                   clear                   og146
009900070529     c                   clear                   og148
010000070529     c                   endif
010100070529     c
010200070529     c                   clear                   FGSdda            8 0
010300070529    1c                   if        §ogdda>*zeros
010400070529     c                   testn                   §ogdda               37
010500070529     c                   move      §ogdda        w001a             1
010600070529    2c                   if        *in37 and w001a>='0'
010700070529     c* Se la data = 20391231, come se non ci fosse
010800070529    3c                   if        §ogdda<>'20391231'
010900070529     c                   movel     §ogdda        FGSdda            8 0
011000070529    3c                   endif
011100070529    2c                   endif
011200070529    1c                   endif
011300070529     c
011400070529    1c                   if        fgsdda=0 or fgsdda>v1cdat
011500070529     c                   seton                                        03
011600070612     c                   else
011700070705     c                   seton                                        05
011800070705     c                   eval      v1cgiro='S'
011900070430     C                   ENDIF
012000060904     c
012100040917     c
012200040917     C     FOR01         TAG
012300000828      *
012400060904     C                   EXFMT     LRL2D01
012500970102     C                   SETOFF                                       289050
012600040917     C* F3 - FINE
012700040917     C   KC              GOTO      FINE
012800041220      *  CONTROLLI
012900040917     C                   EXSR      CTRD01
013000040917     c   90              goto      for01
013100000825      *
013200040917     c*
013300060904     c                   if        not *inkf
013400041220     c                   goto      for01
013500041220     c                   endif
013600040917     C*
013700040917      * F6  - SOTTOMETTO ELABORAZIONE
013800050412    1C     *INKF         IFEQ      *ON
013900101119     c* se numero bolleminimo > 0 faccio un lancio di controllo
014000070612    2c                   if        §vpobotlls>0
014100070529     c                   eval      lrl1contrbl='S'
014200070529     c                   eval      lrl1botlls=§vpobotlls
014300070529     c                   clear                   lrl1Ominimo
014400070529     c                   clear                   lrl1Onumbolle
014500070529     C                   MOVEL     FNLRL1DS      KPJBU
014600070529     c                   call      'FNLRL1R'
014700070529     c                   parm                    KPJBA
014800070529     C
014900070529     c* Se non raggiunto numero minimo di bolle errore
015000070529     c                   movel     kpjbu         fnlrl1DS
015100070612    3c                   if        lrl1Ominimo='S'
015200070529     c                   seton                                        509028
015300070529     C                   MOVEL     MSG(4)        V1cMSG
015400070601     c                   eval      %subst(v1cmsg:14:3)=%editc(lrl1Onumbolle:'Z')
015500070529     c                   goto      for01
015600070612    3C                   endif
015700070612    2C                   ENDIF
015800070529     c
015900070529     c* Sottometto elaborazione
016000070529     c                   clear                   lrl1contrbl
016100070529     c                   clear                   lrl1botlls
016200060904     C                   MOVEL     FNLRL1DS      KPJBU
016300080903     c                   movel     'LRL1'        kcoaz
016400090911     c                   call      'BCH10'
016500080903     c
016600090911     c******             call      'FNLRL1R'
016700060904     c                   parm                    KPJBA
016800070612    1C                   ENDIF
016900050412     c
017000060904     c     fine          tag
017100070124     c
017200070124     c                   clear                   fnlv50ds
017300070124     c                   movel     'C'           d50tla
017400070124     c                   call      'FNLV50R'
017500070124     c                   parm                    fnlv50ds
017600000000     C                   SETON                                        LR
017700900515      *-----------------------------------------------------***********
017800060904      * CONTROLLI                                           *  CTRD01 *
017900900515      *-----------------------------------------------------***********
018000040917     C     CTRD01        BEGSR
018100080204      * PUNTO OPERATIVO                          441184517
018200041220     C                   MOVE      V1CPO         CODFIL            3 0
018300951025     C                   EXSR      CTRFIL
018400070124    1c                   if        *in90
018500041220     c                   goto      endct1
018600041220    1C                   ENDIF
018700080204     c* non può essere un p.o. gestito in £6
018800080204     c                   clear                   fnlv55ds
018900080204     c                   eval      d55tla='L'
019000080204     c                   eval      d55tpt='6'
019100080204     c                   eval      d55lin=codfil
019200080204     c                   eval      d55drf=v1cdat
019300080204     c                   call      'FNLV55R'
019400080204     c                   parm                    fnlv55ds
019500080204     c                   if        d55tfa<>codfil
019600080204     C                   MOVEL     MSG(5)        V1cMSG
019700080204     c                   eval      %subst(v1cmsg:25:3)=%editc(d55tfa: 'X')
019800080204     c                   seton                                        289050
019900080204     c                   goto      endct1
020000080204    1C                   ENDIF
020100070124     c
020200070124     c* Verifico se  abilitato a scambio dati con tellus  se no EDP
020300070124     c                   if        not *in01
020400070529     c                   movel     orgde8        og148
020500070529     c                   movel     orgde6        og146
020600070124     c                   if        §ogtlls=' '
020700070124     c                   seton                                        509028
020800070124     C                   MOVEL     MSG(2)        V1cMSG
020900070124     c                   goto      endct1
021000070124    1C                   ENDIF
021100070529     c
021200070529     c* Verifico se presente data
021300070529     c                   clear                   FGSdda            8 0
021400070529    1c                   if        §ogdda>*zeros
021500070529     c                   testn                   §ogdda               37
021600070529     c                   move      §ogdda        w001a             1
021700070529    2c                   if        *in37 and w001a>='0'
021800070529     c* Se la data = 20391231, come se non ci fosse
021900070529    3c                   if        §ogdda<>'20391231'
022000070529     c                   movel     §ogdda        FGSdda            8 0
022100070529    3c                   endif
022200070529    2c                   endif
022300070529    1c                   endif
022400070529     c
022500070529    1c                   if        fgsdda=0 or fgsdda>v1cdat
022600070529     c* se prima spento e adesso acceso devo riemettere videata
022700070529     c                   if        not *in03
022800070529     c                   seton                                        0390
022900070529     c                   setoff                                       05
023000070529     c                   eval      v1cgiro=' '
023100070529     c                   endif
023200070529     c
023300070529     c                   else
023400070529     c* se prima spento e adesso acceso devo riemettere videata
023500070705     c                   if        not *in05
023600070705     c                   seton                                        0590
023700070705     c                   setoff                                       03
023800070705     c                   eval      v1cgiro='S'
023900070705     C                   ENDIF
024000070529     C                   ENDIF
024100070529     c
024200070124    1C                   ENDIF
024300070406     c
024400070406     c* Verifico integrità versioni giri x filiale corrente
024500070406     c*                  call      'FIDG11R1'
024600070406     c*                  parm      codfil        In_Fil
024700070406     c*                  parm      v1cdat        In_Dat
024800070406     c*                  parm      *blanks       Out_KeyVer
024900070406     c*                  parm      *blanks       Out_Esito
025000070406     c*
025100070406     c*                  if        Out_Esito <> *blanks
025200070406     c*                  seton                                        509028
025300070406     C*                  MOVEL     MSG(3)        V1cMSG
025400070406     c*                  goto      endct1
025500070406     C*                  endif
025600900515      *
025700040917     C     ENDCT1        ENDSR
025800951025     C**************************************************************************
025900951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
026000951025     C**************************************************************************
026100951025     C     CTRFIL        BEGSR
026200020213
026300951025     C     CODFIL        CHAIN     AZORG01L                           34
026400951025    1C  N34ORGFVA        IFNE      ' '
026500951025     C     ORGFAG        ORNE      'A'
026600951025     C     ORGFAG        ANDNE     'F'
026700951025     C                   SETON                                        34
026800951025    1C                   ENDIF
026900070124     c
027000070124    1c                   if        not *in34
027100070124     c                   movel     orgdes        V1DPO
027200070124     c
027300070124   x1C                   ELSE
027400070124     c                   seton                                        509028
027500070124     C                   MOVEL     MSG(1)        V1cMSG
027600070124     c                   goto      endctf
027700070124    1C                   ENDIF
027800070124     c
027900070124     c* Se non sono EDP deve essere p.o. della £1
028000070124     c                   if        not *in01
028100070124     C                   SETOFF                                       90
028200070124     C                   CLEAR                   fnlv50ds
028300070124     C                   MOVEL     KNMUS         D50PRU
028400070124     C                   MOVEL     codFIL        D50FGS
028500070124     C                   CALL      'FNLV50R'
028600070124     C                   PARM                    fnlv50ds
028700070124     C*
028800070124     C     D50ERR        IFNE      ' '
028900070124     C                   MOVEL     D50MSG        V1CMSG
029000070124     C                   SETON                                        509028
029100070124     C                   GOTO      ENDCTF
029200070124     C                   ENDIF
029300070124     C                   ENDIF
029400951025     C*
029500070124     C     endctf        ENDSR
029600020805      **************************************************************************
029700960302** SCHIERA MSG - MESSAGGI DI ERRORE
029800070330Filiale INESISTENTE                                                           01
029900070330Filiale non abilitata allo scambio dati con TELLUS !!!                        02
030000070406Errore di versione giri, avvisare urgentemente il CED !!!                     03
030100070706Presenti soloxxx bolle senza giro: attribuire Manualmente quelle senza giro    04
030200080204La filiale è gestita da xxx: effettuare il lancio con la filiale in gestione   04
