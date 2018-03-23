000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200170816     H* FLLVTVR *-----------------------------------------------------*
000300170816     H*  Aggiorna peso e volumi bolle
000400930126     H*---------------------------------------------------------------*
000500170816     Ffnlvtvd   CF   E             WORKSTN
000600040611     FAZORG01L  IF   E           K DISK
000700080319     d*-----------------
000800170817     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
000900080319     d*-----------------
001000911010     D WLBDAT          DS
001100941221     D  G02DAT                 1      8  0
001200941221     D  G02INV                 9     16  0
001300941221     D  G02ERR                17     17
001400941221     D  G02TGI                18     22  0
001500170816     D fnlvtvds        DS
001600170816     D  vidlnp                 1      3  0
001700170816     D  vidccm                 4     10  0
001800170816     D  comela                11     18  0
001900170816     D  vidpes                19     20
002000170816     D  vidpes1               21     27  1
002100170816     D  vidpes2               28     28
002200170816     D  vidvol                29     30
002300170816     D  vidvol1               31     35  3
002400170817     D  vidvol3               36     36
002500170816     D  vidaggio              37     37
002600170816     D  comela2               38     45  0
002700170817     D  vidpesVDL             46     46
002800170817     D  vidvolVDL             47     47
002900170817     D  vidvol5               48     48
003000170817     D  vidvol6               49     49
003100170817     D  contapb               50     56  0
003200080319     d
003300170816     d cnacods       E DS                  EXTNAME(cnaco00f)
003400040604     D CNCR80        E DS
003500040604     D KPJBA         E DS
003600030722     D UT§DSE0F      E DS
003700170816     D tibs69ds      E DS
003800040614     D*
003900080319     d Dataiso         s               d   datfmt(*iso)
004000080319     d Dataeur         s               d   datfmt(*eur)
004100000000     C*---------------------------------------------------------------*
004200911010     C* INDICATORI
004300911010     C*---------------------------------------------------------------*
004400951115     C*  28    - EMETTE CAMPO MESSAGGI
004500040607     C*  40    - ERRORE
004600911010     C*  90    - ERRORE GENERICO
004700911010     C*---------------------------------------------------------------*
004800000000     C     *ENTRY        PLIST
004900000000     C                   PARM                    KPJBA
005000941221     C*
005100000000     C                   Z-ADD     1             CODUT
005200911010     C                   CALL      'X§PARUT'
005300030722     C                   PARM                    UT§DSE0F
005400920901     C                   MOVEL     REC80         CNCR80
005500941221     C                   MOVEL     RAGUT         RSUT
005600170719
005700900511     C*---------------------------------------------------------------*
005800911008     C                   EXSR      PULIZ
005900971223     C*
006000921123     C     FOR01         TAG
006100911008     C*
006200170816     C                   EXFMT     lvtVD01
006300040607     C*
006400040607     C                   SETOFF                                       409028
006500080319     C                   SETOFF                                       414243
006600170816     C                   SETOFF                                       4445
006700030722     C** F3 - FINE LAVORO
006800040604     C   KC              GOTO      FINE
006900040611     C* CONTROLLO
007000911008     C                   EXSR      CONTR1
007100930127     C* SE ERRORE
007200040607     c  nkf
007300040607     Cor 90              GOTO      FOR01
007400040528     c
007500030722     C* F6  - ELABORAZIONE
007600040607     c                   if        *inkf
007700170816     c                   movel     fnlvtvds      kpjbu
007800170816     c                   call      'FNLVTVR1'
007900170816     c                   parm                    kpjba
008000170817     c                   movel     kpjbu         fnlvtvds
008100170817     c                   if        vidaggio=' '
008200170817     c                   eval      vidmsg=msg(10)
008300170817     c                   else
008400170817     c                   eval      vidmsg=msg(11)
008500170817     c                   endif
008600170817     c                   eval      vidmsg=%trim(vidmsg) + ' ' +
008700170817     c                             %editc(contapb:'2')
008800170817     c                   seton                                        2890
008900170816
009000170816     c                   clear                   forza1            1
009100170816     c                   clear                   forza2            1
009200170817     c                   clear                   vidaggio
009300170719     c                   goto      for01
009400040604     c                   endif
009500040604     c*
009600000000     C     FINE          TAG
009700960202     C*
009800000000     C                   SETON                                        LR
009900941216     C**************************************************************************
010000941216     C*    PULIZIA CAMPI
010100941216     C**************************************************************************
010200911008     C     PULIZ         BEGSR
010300941216     C*
010400040607     C                   TIME                    w0140            14 0
010500080319     C                   MOVE      w0140         videla
010600040528     c
010700040604     C                   TIME                    UTIME             6 0
010800170816     c                   clear                   videla2
010900170816     c                   clear                   vidlnp
011000170816     c                   clear                   vidccm
011100170719     c                   clear                   deslnp
011200170816     c                   clear                   desccm
011300170816     c                   eval      vidpes='SI'
011400170816     c                   eval      vidvol='SI'
011500170816     c                   clear                   vidpes1
011600170816     c                   clear                   vidpes2
011700170816     c                   clear                   vidvol1
011800170817     c                   eval      vidvolvdl=' '
011900170817     c                   eval      vidvol5=' '
012000170817     c                   eval      vidvol6=' '
012100170816     c                   eval      vidvol3=' '
012200170817     c                   eval      vidpesvDL=' '
012300170816     c                   clear                   forza1
012400170816     c                   clear                   forza2
012500170817     c                   clear                   contapb
012600040611     c
012700040607     c*
012800911008     C                   ENDSR
012900960205     C**************************************************************************
013000040607     C*    CONTROLLI data arrivo  colli
013100960205     C**************************************************************************
013200960205     C     CONTR1        BEGSR
013300040607     C*
013400080328     c
013500080328     c* se c'e' la data controllo prima del p.o.
013600080328     c                   clear                   comela
013700170816     c                   clear                   comela2
013800170816
013900080328     C                   Z-ADD     videla        G02DAT
014000080328     C                   CLEAR                   G02ERR
014100080328     C                   CALL      'XSRDA8'
014200080328     C                   PARM                    WLBDAT
014300080328     C*
014400080328     C* Data errata
014500080328    1C     G02ERR        IFEQ      '1'
014600080328     C                   MOVEL     msg(1)        VIDMSG
014700080328     C                   SETON                                        419028
014800080328     c                   goto      endct1
014900080328     c                   else
015000080328     c                   z-add     g02dat        videla
015100080328     c                   z-add     g02inv        comela            8 0
015200080328    1C                   ENDIF
015300170816
015400170816     c* Data AL non obbligatoria
015500170816     c                   if        videla2>0
015600170816     C                   Z-ADD     videla2       G02DAT
015700170816     C                   CLEAR                   G02ERR
015800170816     C                   CALL      'XSRDA8'
015900170816     C                   PARM                    WLBDAT
016000170816     C*
016100170816     C* Data errata
016200170816    1C     G02ERR        IFEQ      '1'
016300170816     C                   MOVEL     msg(1)        VIDMSG
016400170816     C                   SETON                                        449028
016500170816     c                   goto      endct1
016600170816     c                   else
016700170816     c                   z-add     g02dat        videla2
016800170816     c                   z-add     g02inv        comela2           8 0
016900170816    1C                   ENDIF
017000170816     c                   if        comela2<comela
017100170816     C                   MOVEL     msg(7)        VIDMSG
017200170816     C                   SETON                                        449028
017300170816     c                   goto      endct1
017400170816     c                   endif
017500170816     c                   endif
017600170816     c* controllo lnp
017700170719     c                   clear                   deslnp
017800170816     c*
017900170719     c     vidlnp        chain     azorg01l
018000080319     c                   if        not %found(azorg01l)
018100170816     c                             or (orgfag<>'A' and orgfag<>'F')
018200170719     C                   MOVEL     msg(2)        VIDMSG
018300080319     C                   SETON                                        409028
018400080319     c                   goto      endct1
018500080319     c                   else
018600170719     c                   movel     orgdes        deslnp
018700080319     c                   endif
018800170719
018900170719
019000170816     c* cod cliente
019100170816     c                   clear                   desccm
019200170816     c                   clear                   tibs69ds
019300170816     c                   eval      i69tla='L'
019400170816     c                   eval      i69kac=vidccm
019500170816     c                   call      'TIBS69R'
019600170816     c                   parm                    tibs69ds
019700170816     c                   parm                    cnacods
019800170816     C                   IF        o69ERR<>' ' OR ACOflg<>' '
019900170817     C                             OR VIDCCM=0
020000170816     C                   MOVEL     msg(3)        VIDMSG
020100170816     C                   SETON                                        429028
020200170816     c                   goto      endct1
020300170816     c                   endif
020400170816     c                   movel     acorag        desccm
020500170816     c* PESO
020600170816     c                   if        vidpes='SI'
020700170816     c                   if        vidpes1=0 and vidpes2=' '
020800170816     C                   MOVEL     msg(4)        VIDMSG
020900170816     C                   SETON                                        439028
021000170816     c                   goto      endct1
021100170816     c                   endif
021200170816     c                   if        vidpes1>0 and vidpes2='S'
021300170816     C                   MOVEL     msg(4)        VIDMSG
021400170816     C                   SETON                                        439028
021500170816     c                   goto      endct1
021600170816     c                   endif
021700170816     c* Peso > 350 kg --> sei sicuro?
021800170816     c                   if        vidpes1>350 and forza1=' '
021900170816     C                   MOVEL     msg(5)        VIDMSG
022000170816     C                   SETON                                        439028
022100170816     c                   eval      forza1='S'
022200170816     c                   goto      endct1
022300170816     c                   endif
022400170816     c                   endif
022500170816     c* VOLUME
022600170816     c                   if        vidVOL='SI'
022700170816     c* Immettere solo una scelta
022800170817     c                   if        vidvol6=' ' and vidvol3=' '  and
022900170817     C                             vidvolvdl=' ' and vidvol5=' '
023000170817     C                   MOVEL     msg(9)        VIDMSG
023100170816     C                   SETON                                        459028
023200170816     c                   goto      endct1
023300170816     c                   endif
023400170817     c                   if        vidvol3='S'  and (vidvol6='S' or
023500170817     C                             vidvolvdl='S' or  vidvol5='S')
023600170817     C                   MOVEL     msg(9)        VIDMSG
023700170816     C                   SETON                                        459028
023800170816     c                   goto      endct1
023900170816     c                   endif
024000170817     c                   if        vidvol6='S' and
024100170817     C                             vidvolvdl=' ' and vidvol5=' '
024200170817     C                   MOVEL     msg(12)       VIDMSG
024300170817     C                   SETON                                        459028
024400170817     c                   goto      endct1
024500170817     c                   endif
024600170817     c
024700170817     c* se richiesto l'aggiornamento del volume reale, indicare il valore
024800170817     c                   if        vidvol3='S' or vidvol6='S'
024900170817     c                   if        vidvol1=0
025000170817     C                   MOVEL     msg(8)        VIDMSG
025100170817     C                   SETON                                        469028
025200170817     c                   goto      endct1
025300170817     c                   endif
025400170817     c
025500170817     c                   if        vidvol1>2   and forza2=' '
025600170817     C                   MOVEL     msg(6)        VIDMSG
025700170817     C                   SETON                                        469028
025800170817     c                   eval      forza2='S'
025900170817     c                   goto      endct1
026000170817     c                   endif
026100170816     c                   endif
026200170817
026300170817     c                   endif
026400170816     c*
026500040611     C     endct1        ENDSR
026600000606**  MSG
026700080319Data elaborazione errata                                                      1
026800080319Filiale inesistente
026900170816Codice cliente errato
027000170817Effettuare  UNA  scelta per l'aggiornamento del PESO
027100170816Peso da impostare MAGGIORE 350 KG : sei sicuro ???
027200170816Volume da impostare MAGGIORE 2 MC : sei sicuro ???
027300170816Data AL errata perchè MINORE della data DAL
027400170816Immettere il valore del volume da aggiornare
027500170817Aggiornamento VOLUME: scegliere o SEMPRE, o una o più delle altre 3 opzioni
027600170817Totale  bolle  DA  aggiornare :
027700170817Totale  bolle  A G G I O R N A T E :
027800170817Per aggiornare tutti i volumi come REALI selezionare il campo apposito
