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
002400161006     D ds7B          E DS
002500080319     d
002600040611      * Parametri per richiamo a TRUL09R
002700040611     d Trul09ds      e ds
002800040611     d  po2liv                19    108  0 dim(30)
002900080319     d
003000040611     d fnlv55DS      e ds
003100080319     d* Passagio dati a pgm batch
003200080319     d fnlrv7ds      e ds
003300040614     D*
003400080319     d Dataiso         s               d   datfmt(*iso)
003500080319     d Dataeur         s               d   datfmt(*eur)
003600000000     C*---------------------------------------------------------------*
003700911010     C* INDICATORI
003800911010     C*---------------------------------------------------------------*
003900951115     C*  28    - EMETTE CAMPO MESSAGGI
004000040607     C*  40    - ERRORE
004100911010     C*  90    - ERRORE GENERICO
004200911010     C*---------------------------------------------------------------*
004300000000     C     *ENTRY        PLIST
004400000000     C                   PARM                    KPJBA
004500941221     C*
004600000000     C                   Z-ADD     1             CODUT
004700911010     C                   CALL      'X§PARUT'
004800030722     C                   PARM                    UT§DSE0F
004900920901     C                   MOVEL     REC80         CNCR80
005000941221     C                   MOVEL     RAGUT         RSUT
005100900511     C*---------------------------------------------------------------*
005200911008     C                   EXSR      PULIZ
005300971223     C*
005400921123     C     FOR01         TAG
005500911008     C*
005600080319     C                   EXFMT     LRV6D01
005700040607     C*
005800040607     C                   SETOFF                                       409028
005900080319     C                   SETOFF                                       414243
006000030722     C** F3 - FINE LAVORO
006100040604     C   KC              GOTO      FINE
006200921117     C*
006300040611     C* CONTROLLO
006400911008     C                   EXSR      CONTR1
006500930127     C* SE ERRORE
006600040607     c  nkf
006700040607     Cor 90              GOTO      FOR01
006800040528     c
006900030722     C* F6  - ELABORAZIONE
007000040607     c                   if        *inkf
007100040611     c* imposto la DS
007200080319     c                   clear                   fnlrv7ds
007300080319     c                   eval      ilrv7dela=comela
007400080319     c                   eval      ilrv7tbo=vidtbo
007500080319     c                   eval      ilrv7fil=vidfil
007600080319     c                   eval      ilrv7npg=vidnpg
007700080319     c                   eval      ilrv7nfv=vidnfv
007800080319     c                   eval      ilrv7minr=vidminr
007900080403     c                   eval      ilrv7sip =vidsip
008000040611     c
008100080319     c                   movel     fnlrv7ds      kpjbu
008200080319     C                   MOVEL     'LRV7'        KCOAZ
008300040607     C                   CALL      'BCH10'
008400040607     C                   PARM                    KPJBA
008500040604     c                   endif
008600040604     c*
008700000000     C     FINE          TAG
008800040611     c                   clear                   fnlv55ds
008900040611     c                   eval      d55tla='C'
009000040611     C                   CALL      'FNLV55R'
009100040611     C                   PARM                    fnlv55ds
009200960202     C*
009300000000     C                   SETON                                        LR
009400941216     C**************************************************************************
009500941216     C*    PULIZIA CAMPI
009600941216     C**************************************************************************
009700911008     C     PULIZ         BEGSR
009800080328     c                   setoff                                       1001
009900080328    2c                   if        %subst(knmus:1:3) = 'EDP'
010000080328     c                   seton                                        01
010100080328     c                   endif
010200941216     C*
010300920709     C* PRIMA VIDEATA
010400080319     C                   Z-ADD     SIMPOU        vidfil
010500040611     c     simpou        chain     azorg01l
010600080319     c                   movel     orgdes        desfil
010700080403     c
010800080403     c                   eval      vidsip='S'
010900040611     c*
011000080319     c                   eval      vidtbo='A'
011100040607     C                   TIME                    w0140            14 0          ORA E DATA
011200080319     C                   MOVE      w0140         videla                         GG/MM/AAAA
011300080328     c                   movel     videla        dataeur
011400080328     c                   movel     dataeur       dataiso
011500080328     c                   movel     dataiso       amgudate          8 0
011600040528     c
011700040604     C                   TIME                    UTIME             6 0          ORA E DATA
011800040611     c
011900040611     c* Se 2 livello propongo p.o. non modificabile
012000040611     c     simtpp        ifeq      '2'
012100040611     c     simtpp        oreq      *blanks
012200040611     c                   seton                                        10
012300040611     c                   endif
012400080319     c                   eval      vidminr=3
012500080319     c
012600080319     c     kfvv          klist
012700080319     c                   kfld                    vidnpg
012800080319     c                   kfld                    vidnfv
012900080319     c                   kfld                    vidfil
013000080319     c     kbrv          klist
013100080319     c                   kfld                    vidnpg
013200080319     c                   kfld                    knfv              6 0
013300080319     c                   kfld                    vidfil
013400080319     c     kfgv          klist
013500080319     c                   kfld                    vidnfv
013600080319     c                   kfld                    vidfil
013700080319     c     ktab          klist
013800080319     c                   kfld                    tblkut
013900080319     c                   kfld                    tblcod
014000080319     c                   kfld                    tblkey
014100080319     c
014200080319     c                   z-add     1             tblkut
014300040607     c*
014400911008     C                   ENDSR
014500960205     C**************************************************************************
014600040607     C*    CONTROLLI data arrivo  colli
014700960205     C**************************************************************************
014800960205     C     CONTR1        BEGSR
014900040607     C*
015000080319     c* Numero ricorrenze > 0
015100080319    1c                   if        vidminr=0
015200080319     C                   MOVEL     msg(6)        VIDMSG
015300080319     C                   SETON                                        439028
015400080319     c                   goto      endct1
015500080319    1c                   endif
015600080328     c
015700080328     c* se c'e' la data controllo prima del p.o.
015800080328     c                   clear                   comela
015900080328     c                   if        videla>0
016000080328     C                   Z-ADD     videla        G02DAT
016100080328     C                   CLEAR                   G02ERR
016200080328     C                   CALL      'XSRDA8'
016300080328     C                   PARM                    WLBDAT
016400080328     C*
016500080328     C* Data errata
016600080328    1C     G02ERR        IFEQ      '1'
016700080328     C                   MOVEL     msg(1)        VIDMSG
016800080328     C                   SETON                                        419028
016900080328     c                   goto      endct1
017000080328     c                   else
017100080328     c                   z-add     g02dat        videla
017200080328     c                   z-add     g02inv        comela            8 0
017300080328    1C                   ENDIF
017400080328    1C                   ENDIF
017500040611     c* controllo p.o.
017600080319     c                   clear                   desfil
017700080319    1c                   if        vidfil<>0
017800080328     c* Ambito PARTENZE
017900080328   1ac                   if        not *in01
018000080328     c
018100080319    2c                   if        vidtbo='P'
018200080328     c
018300040611     c* Deve essere un p.o. del terminal di partenza del p.o. gestione
018400040611     c                   clear                   fnlv55ds
018500040611     C                   MOVEL     'P'           D55TPT
018600080319     C                   MOVEL     vidfil        D55LIN
018700080328     c                   if        comela>0
018800080328     C                   MOVEL     comela        D55DRF
018900080328     c                   else
019000080328     C                   MOVEL     amgudate      D55DRF
019100080328     c                   endif
019200040611     C                   CALL      'FNLV55R'
019300040611     C                   PARM                    fnlv55ds
019400040611     c*
019500080319    3c                   if        d55err<>*blanks or d55tfp<>simfel
019600040611     C                   MOVEL     msg(2)        VIDMSG
019700040611     C                   SETON                                        409028
019800080319     c                   goto      endct1
019900080319    3c                   endif
020000040611     c*
020100040611   x2c                   else
020200080328     c* Ambito ARRIVI
020300040614     c* carico tutti i p.o. 2 livello in arrivo del terminal
020400040611     c                   clear                   trul09ds
020500040611     c                   eval      i09mod='TFA'
020600080319     c                   eval      i09tfa=simfel
020700040611     c                   eval      i09tla='L'
020800080328     c                   if        comela>0
020900080328     c                   eval      i09dta=comela
021000080328     c                   else
021100080328     c                   eval      i09dta=amgudate
021200080328     c                   endif
021300040611     c                   call      'TRUL09R'
021400040611     c                   parm                    trul09ds
021500040611     c
021600040611     c     vidfil        lookup    po2liv                                 30
021700040611    3c                   if        not *in30
021800040611     C                   MOVEL     msg(3)        VIDMSG
021900040611     C                   SETON                                        409028
022000080319     c                   goto      endct1
022100040611    3c                   endif
022200040611    2c                   endif
022300080328   1ac                   endif
022400080328     c
022500080319     c* decodifico la filiale
022600080319     c     vidfil        chain     azorg01l
022700080319     c                   if        not %found(azorg01l)
022800080319     C                   MOVEL     msg(8)        VIDMSG
022900080319     C                   SETON                                        409028
023000080319     c                   goto      endct1
023100080319     c                   else
023200080319     c                   movel     orgdes        desfil
023300080319     c                   endif
023400080319     c
023500080319   x1c                   else
023600080319     c* solo EDP può lasciare la filiale vuota
023700080319    2c                   if        %subst(knmus:1:3) <>'EDP'
023800080319     C                   MOVEL     msg(7)        VIDMSG
023900080319     C                   SETON                                        409028
024000080319     c                   goto      endct1
024100080319    3c                   endif
024200040611    1c                   endif
024300080319     c
024400080328     C* Data elaborazione o foglio obbligatori
024500080328     c                   if        videla=0 and vidnfv=0
024600080328     C                   MOVEL     msg(10)       VIDMSG
024700080328     C                   SETON                                        419028
024800080328     c                   goto      endct1
024900080328     c                   endif
025000080328     c
025100080328     c
025200080319     c* Se immesso numero foglio lo controllo
025300080319     c                   clear                   desnfv
025400080319     c                   clear                   descons
025500080319     c                   clear                   wspg
025600080319    1c                   if        vidnfv>0
025700080328     c* filiale obbligatoria
025800080328    2c                   if        vidfil=0
025900080328     C                   MOVEL     msg(11)       VIDMSG
026000080328     C                   SETON                                        409028
026100080328     c                   goto      endct1
026200080328    2c                   endif
026300080319     c* Partenza
026400080319    2c                   if        vidnpg=1
026500080319     c     kfgv          chain     fnfgv01l
026600080319    3c                   if        not %found(fnfgv01l)
026700080319     C                   MOVEL     msg(4)        VIDMSG
026800080319     C                   SETON                                        429028
026900080319     c                   goto      endct1
027000080319    3c                   endif
027100080319     c                   movel     fgvdfv        wdfv              8 0
027200080319     c                   clear                   wspg
027300080319     c
027400080319   x2c                   else
027500080319     c     kfvv          chain     fnfvv01l
027600080319    3c                   if        not %found(fnfvv01l)
027700080319     C                   MOVEL     msg(4)        VIDMSG
027800080319     C                   SETON                                        429028
027900080319     c                   goto      endct1
028000080319    3c                   endif
028100080319     c
028200080319     c                   movel     fvvdfv        wdfv              8 0
028300080319     c                   movel     fvvspg        wspg              1
028400080319    2c                   endif
028500080319     c
028600080319     c*  Controllo se ha spunte
028700080319     c                   z-add     vidnfv        knfv
028800080319     c     kbrv          chain     fnbrv01l
028900080319    2c                   if        not %found(fnbrv01l)
029000080319     C                   MOVEL     msg(5)        VIDMSG
029100080319     C                   SETON                                        429028
029200080319     c                   goto      endct1
029300080319    2c                   endif
029400080319     c
029500080319     c                   movel     wdfv          dataiso
029600080319     c                   movel     dataiso       dataeur
029700080319     c                   movel     dataeur       wdfv
029800080319     c
029900080319     c                   movel     '7N'          tblcod
030000080319     c                   movel(P)  vidnpg        tblkey
030100080319     c     ktab          chain     tabel
030200080319    3c                   if        %found(tabel00f)
030300080319     c                   movel     tbluni        ds7n
030400080319     c                   else
030500080319     c                   clear                   ds7n
030600080319    3c                   endif
030700080319     C
030800080319    2c                   if        wspg=' '
030900080319     c                   eval      desnfv=§7ndes
031000080319   x2c                   else
031100161006     c* descrizione breve dalla SOTTOCATEGORIA
031200161006     c                   movel     '7B'          tblcod
031300161006     c                   movel(P)  wspg          tblkey
031400161006     c     ktab          chain     tabel
031500161006    3c                   if        %found(tabel00f)
031600161006     c                   movel     tbluni        ds7b
031700161006     c                   else
031800161006     c                   clear                   ds7b
031900161006    3c                   endif
032000161006     c                   eval      desnfv=§7bdesb
032100161006     c
032200161006     c**                 eval      desnfv='I.M. '
032300161006     c**                 eval      desnfv=%trim(desnfv)+wspg+'.'
032400080319    2c                   endif
032500080319     c
032600080319     c                   eval      desnfv=%trim(desnfv)+' del ' +
032700080319     c                             %editw(wdfv: '  /  /    ')
032800080319     c
032900080319    2c                   if        vidnpg=4 or vidnpg=8
033000080319     c                   eval      descons=fvvdsf
033100080319    2c                   endif
033200080319     c
033300080319     c* controllo congruenza con l'ambito indicato
033400080319    2c                   if        WSPG=' ' AND VIDTBO<>§7NTBO
033500080319     C                   MOVEL     msg(9)        VIDMSG
033600080319     C                   SETON                                        429028
033700080319     c                   goto      endct1
033800080319    2c                   endif
033900080319    3c                   if        wspg<>' '
034000080319    3c                   if        (wspg=§7nsc1 and vidtbo<>§7ntb1) or
034100080319     c                             (wspg=§7nsc2 and vidtbo<>§7ntb2) or
034200080319     c                             (wspg=§7nsc3 and vidtbo<>§7ntb3) or
034300080319     c                             (wspg=§7nsc4 and vidtbo<>§7ntb4) or
034400080319     c                             (wspg=§7nsc5 and vidtbo<>§7ntb5) or
034500080319     c                             (wspg=§7nsc6 and vidtbo<>§7ntb6)
034600080319     C                   MOVEL     msg(9)        VIDMSG
034700080319     C                   SETON                                        429028
034800080319     c                   goto      endct1
034900080319    3c                   endif
035000080319    2c                   endif
035100080319    1c                   endif
035200040611     c
035300040611     C     endct1        ENDSR
035400000606**  MSG
035500080319Data elaborazione errata                                                      1
035600080319La filiale non è un 2 livello in partenza
035700080319La filiale non è un 2 livello in arrivo
035800080319Numero foglio non trovato
035900080319Non trovate spunte sul foglio indicato: controllo non effettuabile
036000080319Numero minimo ricorrenze spunte doppie deve essere maggiore di zero
036100080319Filiale spunta obbligatoria
036200080319Filiale inesistente
036300080319Ambito di elaborazione incongruente con la categoria del foglio
036400080328Immettere la "Data Elaborazione" o il  "Numero Foglio"
036500080328Filiale obbligatoria se indicato il numero del Foglio
