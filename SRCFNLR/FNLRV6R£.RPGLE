000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200080319     H* FNLRV6R *-----------------------------------------------------*
000300080319     H*   Lista di controllo spunte doppie
000400930126     H*---------------------------------------------------------------*
000500080319     FFNLRv6d   CF   E             WORKSTN
000600040611     FAZORG01L  IF   E           K DISK
000700080319     Ffnfgv01l  IF   E           K DISK
000800080319     Ffnfvv01l  IF   E           K DISK
000900080319     Ffnbrv01l  IF   E           K DISK
001000080319     Ftabel00f  IF   E           K DISK
001100080319     d*-----------------
001200080328     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             MESSAGGI ERRORE
001300080319     d*-----------------
001400911010     D WLBDAT          DS
001500941221     D  G02DAT                 1      8  0
001600941221     D  G02INV                 9     16  0
001700941221     D  G02ERR                17     17
001800941221     D  G02TGI                18     22  0
001900080319     d
002000040604     D CNCR80        E DS
002100040604     D KPJBA         E DS
002200030722     D UT§DSE0F      E DS
002300080319     D ds7N          E DS
002400080319     d
002500040611      * Parametri per richiamo a TRUL09R
002600040611     d Trul09ds      e ds
002700040611     d  po2liv                19    108  0 dim(30)
002800080319     d
002900040611     d fnlv55DS      e ds
003000080319     d* Passagio dati a pgm batch
003100080319     d fnlrv7ds      e ds
003200040614     D*
003300080319     d Dataiso         s               d   datfmt(*iso)
003400080319     d Dataeur         s               d   datfmt(*eur)
003500000000     C*---------------------------------------------------------------*
003600911010     C* INDICATORI
003700911010     C*---------------------------------------------------------------*
003800951115     C*  28    - EMETTE CAMPO MESSAGGI
003900040607     C*  40    - ERRORE
004000911010     C*  90    - ERRORE GENERICO
004100911010     C*---------------------------------------------------------------*
004200000000     C     *ENTRY        PLIST
004300000000     C                   PARM                    KPJBA
004400941221     C*
004500000000     C                   Z-ADD     1             CODUT
004600911010     C                   CALL      'X§PARUT'
004700030722     C                   PARM                    UT§DSE0F
004800920901     C                   MOVEL     REC80         CNCR80
004900941221     C                   MOVEL     RAGUT         RSUT
005000900511     C*---------------------------------------------------------------*
005100911008     C                   EXSR      PULIZ
005200971223     C*
005300921123     C     FOR01         TAG
005400911008     C*
005500080319     C                   EXFMT     LRV6D01
005600040607     C*
005700040607     C                   SETOFF                                       409028
005800080319     C                   SETOFF                                       414243
005900030722     C** F3 - FINE LAVORO
006000040604     C   KC              GOTO      FINE
006100921117     C*
006200040611     C* CONTROLLO
006300911008     C                   EXSR      CONTR1
006400930127     C* SE ERRORE
006500040607     c  nkf
006600040607     Cor 90              GOTO      FOR01
006700040528     c
006800030722     C* F6  - ELABORAZIONE
006900040607     c                   if        *inkf
007000040611     c* imposto la DS
007100080319     c                   clear                   fnlrv7ds
007200080319     c                   eval      ilrv7dela=comela
007300080319     c                   eval      ilrv7tbo=vidtbo
007400080319     c                   eval      ilrv7fil=vidfil
007500080319     c                   eval      ilrv7npg=vidnpg
007600080319     c                   eval      ilrv7nfv=vidnfv
007700080319     c                   eval      ilrv7minr=vidminr
007800080403     c                   eval      ilrv7sip =vidsip
007900040611     c
008000080319     c                   movel     fnlrv7ds      kpjbu
008100080319     C                   MOVEL     'LRV7'        KCOAZ
008200040607     C                   CALL      'BCH10'
008300040607     C                   PARM                    KPJBA
008400040604     c                   endif
008500040604     c*
008600000000     C     FINE          TAG
008700040611     c                   clear                   fnlv55ds
008800040611     c                   eval      d55tla='C'
008900040611     C                   CALL      'FNLV55R'
009000040611     C                   PARM                    fnlv55ds
009100960202     C*
009200000000     C                   SETON                                        LR
009300941216     C**************************************************************************
009400941216     C*    PULIZIA CAMPI
009500941216     C**************************************************************************
009600911008     C     PULIZ         BEGSR
009700080328     c                   setoff                                       1001
009800080328    2c                   if        %subst(knmus:1:3) = 'EDP'
009900080328     c                   seton                                        01
010000080328     c                   endif
010100941216     C*
010200920709     C* PRIMA VIDEATA
010300080319     C                   Z-ADD     SIMPOU        vidfil
010400040611     c     simpou        chain     azorg01l
010500080319     c                   movel     orgdes        desfil
010600080403     c
010700080403     c                   eval      vidsip='S'
010800040611     c*
010900080319     c                   eval      vidtbo='A'
011000040607     C                   TIME                    w0140            14 0          ORA E DATA
011100080319     C                   MOVE      w0140         videla                         GG/MM/AAAA
011200080328     c                   movel     videla        dataeur
011300080328     c                   movel     dataeur       dataiso
011400080328     c                   movel     dataiso       amgudate          8 0
011500040528     c
011600040604     C                   TIME                    UTIME             6 0          ORA E DATA
011700040611     c
011800040611     c* Se 2 livello propongo p.o. non modificabile
011900040611     c     simtpp        ifeq      '2'
012000040611     c     simtpp        oreq      *blanks
012100040611     c                   seton                                        10
012200040611     c                   endif
012300080319     c                   eval      vidminr=3
012400080319     c
012500080319     c     kfvv          klist
012600080319     c                   kfld                    vidnpg
012700080319     c                   kfld                    vidnfv
012800080319     c                   kfld                    vidfil
012900080319     c     kbrv          klist
013000080319     c                   kfld                    vidnpg
013100080319     c                   kfld                    knfv              6 0
013200080319     c                   kfld                    vidfil
013300080319     c     kfgv          klist
013400080319     c                   kfld                    vidnfv
013500080319     c                   kfld                    vidfil
013600080319     c     ktab          klist
013700080319     c                   kfld                    tblkut
013800080319     c                   kfld                    tblcod
013900080319     c                   kfld                    tblkey
014000080319     c
014100080319     c                   z-add     1             tblkut
014200040607     c*
014300911008     C                   ENDSR
014400960205     C**************************************************************************
014500040607     C*    CONTROLLI data arrivo  colli
014600960205     C**************************************************************************
014700960205     C     CONTR1        BEGSR
014800040607     C*
014900080319     c* Numero ricorrenze > 0
015000080319    1c                   if        vidminr=0
015100080319     C                   MOVEL     msg(6)        VIDMSG
015200080319     C                   SETON                                        439028
015300080319     c                   goto      endct1
015400080319    1c                   endif
015500080328     c
015600080328     c* se c'e' la data controllo prima del p.o.
015700080328     c                   clear                   comela
015800080328     c                   if        videla>0
015900080328     C                   Z-ADD     videla        G02DAT
016000080328     C                   CLEAR                   G02ERR
016100080328     C                   CALL      'XSRDA8'
016200080328     C                   PARM                    WLBDAT
016300080328     C*
016400080328     C* Data errata
016500080328    1C     G02ERR        IFEQ      '1'
016600080328     C                   MOVEL     msg(1)        VIDMSG
016700080328     C                   SETON                                        419028
016800080328     c                   goto      endct1
016900080328     c                   else
017000080328     c                   z-add     g02dat        videla
017100080328     c                   z-add     g02inv        comela            8 0
017200080328    1C                   ENDIF
017300080328    1C                   ENDIF
017400040611     c* controllo p.o.
017500080319     c                   clear                   desfil
017600080319    1c                   if        vidfil<>0
017700080328     c* Ambito PARTENZE
017800080328   1ac                   if        not *in01
017900080328     c
018000080319    2c                   if        vidtbo='P'
018100080328     c
018200040611     c* Deve essere un p.o. del terminal di partenza del p.o. gestione
018300040611     c                   clear                   fnlv55ds
018400040611     C                   MOVEL     'P'           D55TPT
018500080319     C                   MOVEL     vidfil        D55LIN
018600080328     c                   if        comela>0
018700080328     C                   MOVEL     comela        D55DRF
018800080328     c                   else
018900080328     C                   MOVEL     amgudate      D55DRF
019000080328     c                   endif
019100040611     C                   CALL      'FNLV55R'
019200040611     C                   PARM                    fnlv55ds
019300040611     c*
019400080319    3c                   if        d55err<>*blanks or d55tfp<>simfel
019500040611     C                   MOVEL     msg(2)        VIDMSG
019600040611     C                   SETON                                        409028
019700080319     c                   goto      endct1
019800080319    3c                   endif
019900040611     c*
020000040611   x2c                   else
020100080328     c* Ambito ARRIVI
020200040614     c* carico tutti i p.o. 2 livello in arrivo del terminal
020300040611     c                   clear                   trul09ds
020400040611     c                   eval      i09mod='TFA'
020500080319     c                   eval      i09tfa=simfel
020600040611     c                   eval      i09tla='L'
020700080328     c                   if        comela>0
020800080328     c                   eval      i09dta=comela
020900080328     c                   else
021000080328     c                   eval      i09dta=amgudate
021100080328     c                   endif
021200040611     c                   call      'TRUL09R'
021300040611     c                   parm                    trul09ds
021400040611     c
021500040611     c     vidfil        lookup    po2liv                                 30
021600040611    3c                   if        not *in30
021700040611     C                   MOVEL     msg(3)        VIDMSG
021800040611     C                   SETON                                        409028
021900080319     c                   goto      endct1
022000040611    3c                   endif
022100040611    2c                   endif
022200080328   1ac                   endif
022300080328     c
022400080319     c* decodifico la filiale
022500080319     c     vidfil        chain     azorg01l
022600080319     c                   if        not %found(azorg01l)
022700080319     C                   MOVEL     msg(8)        VIDMSG
022800080319     C                   SETON                                        409028
022900080319     c                   goto      endct1
023000080319     c                   else
023100080319     c                   movel     orgdes        desfil
023200080319     c                   endif
023300080319     c
023400080319   x1c                   else
023500080319     c* solo EDP può lasciare la filiale vuota
023600080319    2c                   if        %subst(knmus:1:3) <>'EDP'
023700080319     C                   MOVEL     msg(7)        VIDMSG
023800080319     C                   SETON                                        409028
023900080319     c                   goto      endct1
024000080319    3c                   endif
024100040611    1c                   endif
024200080319     c
024300080328     C* Data elaborazione o foglio obbligatori
024400080328     c                   if        videla=0 and vidnfv=0
024500080328     C                   MOVEL     msg(10)       VIDMSG
024600080328     C                   SETON                                        419028
024700080328     c                   goto      endct1
024800080328     c                   endif
024900080328     c
025000080328     c
025100080319     c* Se immesso numero foglio lo controllo
025200080319     c                   clear                   desnfv
025300080319     c                   clear                   descons
025400080319     c                   clear                   wspg
025500080319    1c                   if        vidnfv>0
025600080328     c* filiale obbligatoria
025700080328    2c                   if        vidfil=0
025800080328     C                   MOVEL     msg(11)       VIDMSG
025900080328     C                   SETON                                        409028
026000080328     c                   goto      endct1
026100080328    2c                   endif
026200080319     c* Partenza
026300080319    2c                   if        vidnpg=1
026400080319     c     kfgv          chain     fnfgv01l
026500080319    3c                   if        not %found(fnfgv01l)
026600080319     C                   MOVEL     msg(4)        VIDMSG
026700080319     C                   SETON                                        429028
026800080319     c                   goto      endct1
026900080319    3c                   endif
027000080319     c                   movel     fgvdfv        wdfv              8 0
027100080319     c                   clear                   wspg
027200080319     c
027300080319   x2c                   else
027400080319     c     kfvv          chain     fnfvv01l
027500080319    3c                   if        not %found(fnfvv01l)
027600080319     C                   MOVEL     msg(4)        VIDMSG
027700080319     C                   SETON                                        429028
027800080319     c                   goto      endct1
027900080319    3c                   endif
028000080319     c
028100080319     c                   movel     fvvdfv        wdfv              8 0
028200080319     c                   movel     fvvspg        wspg              1
028300080319    2c                   endif
028400080319     c
028500080319     c*  Controllo se ha spunte
028600080319     c                   z-add     vidnfv        knfv
028700080319     c     kbrv          chain     fnbrv01l
028800080319    2c                   if        not %found(fnbrv01l)
028900080319     C                   MOVEL     msg(5)        VIDMSG
029000080319     C                   SETON                                        429028
029100080319     c                   goto      endct1
029200080319    2c                   endif
029300080319     c
029400080319     c                   movel     wdfv          dataiso
029500080319     c                   movel     dataiso       dataeur
029600080319     c                   movel     dataeur       wdfv
029700080319     c
029800080319     c                   movel     '7N'          tblcod
029900080319     c                   movel(P)  vidnpg        tblkey
030000080319     c     ktab          chain     tabel
030100080319    3c                   if        %found(tabel00f)
030200080319     c                   movel     tbluni        ds7n
030300080319     c                   else
030400080319     c                   clear                   ds7n
030500080319    3c                   endif
030600080319     C
030700080319    2c                   if        wspg=' '
030800080319     c                   eval      desnfv=§7ndes
030900080319   x2c                   else
031000080319     c                   eval      desnfv='I.M. '
031100080319     c                   eval      desnfv=%trim(desnfv)+wspg+'.'
031200080319    2c                   endif
031300080319     c
031400080319     c                   eval      desnfv=%trim(desnfv)+' del ' +
031500080319     c                             %editw(wdfv: '  /  /    ')
031600080319     c
031700080319    2c                   if        vidnpg=4 or vidnpg=8
031800080319     c                   eval      descons=fvvdsf
031900080319    2c                   endif
032000080319     c
032100080319     c* controllo congruenza con l'ambito indicato
032200080319    2c                   if        WSPG=' ' AND VIDTBO<>§7NTBO
032300080319     C                   MOVEL     msg(9)        VIDMSG
032400080319     C                   SETON                                        429028
032500080319     c                   goto      endct1
032600080319    2c                   endif
032700080319    3c                   if        wspg<>' '
032800080319    3c                   if        (wspg=§7nsc1 and vidtbo<>§7ntb1) or
032900080319     c                             (wspg=§7nsc2 and vidtbo<>§7ntb2) or
033000080319     c                             (wspg=§7nsc3 and vidtbo<>§7ntb3) or
033100080319     c                             (wspg=§7nsc4 and vidtbo<>§7ntb4) or
033200080319     c                             (wspg=§7nsc5 and vidtbo<>§7ntb5) or
033300080319     c                             (wspg=§7nsc6 and vidtbo<>§7ntb6)
033400080319     C                   MOVEL     msg(9)        VIDMSG
033500080319     C                   SETON                                        429028
033600080319     c                   goto      endct1
033700080319    3c                   endif
033800080319    2c                   endif
033900080319    1c                   endif
034000040611     c
034100040611     C     endct1        ENDSR
034200000606**  MSG
034300080319Data elaborazione errata                                                      1
034400080319La filiale non è un 2 livello in partenza
034500080319La filiale non è un 2 livello in arrivo
034600080319Numero foglio non trovato
034700080319Non trovate spunte sul foglio indicato: controllo non effettuabile
034800080319Numero minimo ricorrenze spunte doppie deve essere maggiore di zero
034900080319Filiale spunta obbligatoria
035000080319Filiale inesistente
035100080319Ambito di elaborazione incongruente con la categoria del foglio
035200080328Immettere la "Data Elaborazione" o il  "Numero Foglio"
035300080328Filiale obbligatoria se indicato il numero del Foglio
