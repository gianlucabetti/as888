000100000710     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300030708      * FNLRF7R1*                                                    *
000400950524      *---------*                                                    *
000500950524      *         - PREPARAZIONE DISTINTA DI CONSEGNA -                *
000600030708      *         --        DA SPUNTE (SOLO POSTE    --                *
000700950524      *--------------------------------------------------------------*
000800070601     F*NARB70L  UF   E           K DISK    COMMIT
000900980429     FFNARB01L  IF   E           K DISK
001000980429     F                                     RENAME(FNARB000:FNARB001)
001100070129     FFNBRV01L  IF   E           K DISK
001200080507     FFidst09L  IF   E           K DISK    prefix(£)
001300080507     FFidsf02L  IF   E           K DISK
001400090303     Ffnlrf71L  UF   E           K DISK
001500030708     FFNFVV02L  UF   E           K DISK    COMMIT
001600030708     FFNFVV01L  UF   E           K DISK    COMMIT
001700950613     F                                     RENAME(FNFVV000:FNFVV001)
001800950915     FFNART01L  IF   E           K DISK
001900950915     FFNLBL01L  IF   E           K DISK
002000021204     Ffiapd01L  IF   E           K DISK
002100990519     FFNANM01L  IF   E           K DISK
002200990519     F                                     INFDS(FNANM1)
002300131218     FFNEVB04L  IF   E           K DISK
002400130326     FwFNEVB00F O  A E           K DISK
002500990803     F                                     RENAME(FNEVB000:FNEVBPRV)
002600040827     Fazorg01l  IF   E           K DISK
002700030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
002800950613     F                                     USROPN
002900950524     D*--------------------------------------------------------------*
003000950524     D*  DS
003100950524     D*--------------------------------------------------------------*
003200020916     D ddatiute      e ds
003300020916     D azuteds       e ds                  extname(AZUTE00F)
003400020916     D tibs34ds      E DS                  inz
003500080507     D ddstflr       E DS                  inz
003600080507     D OG148         E DS
003700950613     D WLBDA8          DS                  INZ
003800950613     D  G02DAT                 1      8  0
003900950613     D  G02INV                 9     16  0
004000950613     D  G02ERR                17     17
004100950613     D  G02TGI                18     22  0
004200950524     D KPJBA         E DS
004300950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
004400960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
004500990519     D FNANM1          DS
004600960415     D  ANMNR1               397    400B 0
004700131104      * ds interne pgm
004800131104     c*
004900131104     d dEVB01        e ds
005000131104     c*
005100030729     D CMAT            C                   CONST('MATTINO   ')
005200030729     D CPOM            C                   CONST('POMERIGGIO')
005300950524     C*--------------------------------------------------------------*
005400950524     C*  CICLO PRINCIPALE
005500950524     C*--------------------------------------------------------------*
005600950613     C     *ENTRY        PLIST
005700950613     C                   PARM                    KPJBA
005800950613     C                   MOVEL     KPJBU         FNLR59
005900080507     c* leggo organigramma x test pda
006000080507     c     d59fgs        chain     azorg01l
006100080507     c                   if        %found(azorg01l)
006200080507     c                   movel     orgde8        og148
006300080507     c                   else
006400080507     c                   clear                   og148
006500080507     c                   end
006600040930     c*
006700040930     c                   movel(p)  'FNLRF7R'     nompgm           10
006800050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
006900050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
007000050204     C* AUTOTRASPORTATORE
007100050204     C                   IF        D59STP = 'A'
007200050204     C                   SETON                                        61
007300050204     C                   SETOFF                                       62
007400050204     C                   ELSE
007500050204     C                   SETOFF                                       61
007600050204     C                   SETON                                        62
007700050204     C                   END
007800030718     c*
007900030718     C     D59TPR        IFeq      'U'
008000030718     c                   seton                                        lr
008100030718     c                   return
008200030718     c                   end
008300030718     c*
008400950613     C                   SETON                                        30
008500950613      *  Imposto la chiave per il posizionamento
008600950613     C                   Z-ADD     4             KNPG
008700950613     C                   Z-ADD     D59FGS        KFGS
008800031016      * mi posiziono sul foglio
008900950613     C                   SETOFF                                       30
009000950613     C                   Z-ADD     D59NDD        KNFV
009100000114     C                   Z-ADD     D59NDD        KNFV1
009200950613     C     KFVV1         CHAIN     FNFVV01L                           3132
009300030909     c                   if        *in32
009400030909     C     KFVV1         CHAIN(n)  FNFVV01L
009500101201     c                   if        d59sqa = ' ' or
009600101201     c                             d59sqa = 'T'
009700080507     C                   EXSR      stampa
009800080507     C                   EXSR      ALLOC
009900030909     c                   end
010000101125     c                   end
010100000912      *
010200950613     C     *IN31         IFEQ      '0'
010300950613     C     *IN32         ANDEQ     '0'
010400030725     C     FVVDFV        ANDEQ     D59DDT                                       F.V.chiuso +
010500030725     C     FVVFCF        ANDNE     'S'                                          F.V.chiuso +
010600000802     C     FVVATB        ANDNE     '*'                                          F.V.chiuso +
010700030721      * verifico padroncino
010800110518     C     KDST9         CHAIN     FiDST09L                           99
010900070601     C  N99kapd          CHAIN     fiapd01L                           99
011000070601     C     *IN99         IFEQ      '0'
011100950613     C                   EXSR      AZZDIS
011200030724     C                   ELSE
011300030724     C                   UNLOCK    FNFVV01L
011400030721     C                   END
011500000912     C                   ELSE
011600030721      * deve disallocare xchè il pgm chiude in rt e il record lo alloca
011700030721      * anche il pgm principale
011800000913     C                   UNLOCK    FNFVV01L
011900950613     C                   END
012000950613     C*
012100980310     C                   Z-ADD     WDAY          D59AMG
012200020916     C                   MOVE      DUTCOU        D59CDU
012300950613     C                   MOVEL     FNLR59        KPJBU
012400950613     C                   SETON                                        RT
012500950613     C*-----------------------------------------------------*
012600950613     C*  Routine per azzeramento del numero distinta
012700950613     C*-----------------------------------------------------*
012800950613     C     AZZDIS        BEGSR
012900950613     C*
013000950613     C*  Controllo se il foglio viaggio è bloccato
013100950613     C     FVVFCF        IFEQ      'A'
013200950613     C     FVVFCF        OREQ      'F'
013300030721     C  n30              UNLOCK    FNFVV01L
013400030721     C   30              unlock    fnfvv02l
013500110112     c                   if        d59sqa = ' ' or
013600110112     c                             d59sqa = 'T'
013700080507     C                   EXSR      stampa
013800080507     C                   EXSR      ALLOC
013900110112     C                   END
014000950613     C                   ELSE
014100950613     C                   EXSR      GESDIS
014200950613     C                   END
014300950613     C*  Azzero campi di stampa x record bloccato
014400020917     C                   Z-ADD     0             FVVfgs
014500020917     C                   Z-ADD     0             FVVNFV
014600950613     C                   Z-ADD     0             FVVDFV
014700950613     C                   MOVEL     *BLANKS       FVVDSF
014800950613     C*
014900950613     C                   ENDSR
015000950613     C*-----------------------------------------------------*
015100950613     C*  Gestione sinogola distinta
015200950613     C*-----------------------------------------------------*
015300950613     C     GESDIS        BEGSR
015400950613     C*
015500950613     C*  Se non è bloccata lo blocco io
015600950613     C                   Z-ADD     FVVNFV        KNFV
015700000114     C                   Z-ADD     FVVNFV        KNFV1
015800030825     C*
015900070129     C     KBRV          CHAIN     FNBRV01L                           33
016000031017     C* Se sono nel pgm "da spunte" se il foglio non ha spunte lo sblocco
016100031017     c                   if        *in33 and (kcdaz='LR59' or kcdaz='LR60')
016200031017     C  n30              UNLOCK    FNFVV01L
016300031017     C   30              unlock    fnfvv02l
016400031017     c                   else
016500950613     C                   MOVEL     '§'           FVVFCF
016600950613     C  N30              EXCEPT    AGGFL1
016700950613     C   30              EXCEPT    AGGFL2
016800031016     c                   end
016900080507     c* segnalo anomalia se distinta pda e non passati i dati
017000080507     c                   if        §OGPDAcon <> ' '
017100080507     c     kdst9         chain     fidst09l
017200080507     c                   if        %found(fidst09l)
017300110518     c                   movel     £dstpdr       prtpdr
017400080507     c                   movel     £dstflr       ddstflr
017500080507     c                   if        (£dstpda =  'C' or £dstpda = 'E' or
017600080507     c                             §dsttstpda = 'C' or
017700080507     c                             §dsttstpda = 'E')
017800080507     c     kdsf          chain     fidsf02l
017900101201     c                   if        not %found(fidsf02l) and
018000101201     c                             (d59sqa = ' ' or
018100101201     c                             d59sqa = 'T')
018200080507     c                   exsr      stampa
018300080507     c                   exsr      srpda
018400080507     c                   end
018500080507     c                   end
018600080507     c                   end
018700080507     c                   end
018800090223     c* cancello tutti i record di anomalia scritti in eventuali elaboraz.
018900090223     c* precedenti
019000090303     c     kwf           setll     fnlrf71l
019100090223     c                   do        *hival
019200090303     c     kwf           reade     fnlrf71l
019300090303     c                   if        %eof(fnlrf71l)
019400090223     c                   leave
019500090223     c                   end
019600090303     c                   delete    fnlrf71l
019700090223     c                   enddo
019800070601     c* si utilizzerà il pgm "da spunte" solo per accodare e mai per pulire
019900031020     C* sfleggo i record di FNARB che hanno
020000031020     C* il numero distinta uguale a quella che sto preparando.
020100070601     C*                  Z-ADD     FVVFGS        KIFP
020200070601     C*                  Z-ADD     FVVNFV        KNDC
020300070601     C*                  Z-ADD     FVVDFV        KDDC
020400070601     C*    KARB          CHAIN     FNARB70L                           33
020500070601     C*    *IN33         DOWEQ     '0'
020600070601     c*                  movel     arbaas        arbdsp            8 0
020700070601     c*                  move      arbmgs        arbdsp
020800031016      *FDC = S TRATTASI DI DISTINTA CHIUSA
020900070530     c* abblenco i riferimenti della distinta se pgm "da spunte"
021000070601     c*                  if        arbngd=0 and arbfdc <> 'S' and
021100070601     c*                            (kcdaz='LR59' or kcdaz='LR60')
021200980311     C*  Scrivo in forma provv.l'evento NIC (esclusa dalla consegna)
021300070601     C*                  MOVE      ARBPDC        WCPDC             4 0
021400070601     C*    ARBDCM        IFEQ      0
021500070601     C*                  EXSR      WRTNIC
021600070601     C*                  END
021700070601     C*                  Z-ADD     0             ARBNSS
021800070601     C*                  Z-ADD     0             ARBIFP
021900070601     C*                  Z-ADD     0             ARBNDC
022000070601     C*                  Z-ADD     0             ARBDDC
022100070601     C*                  Z-ADD     0             ARBSTP
022200070601     C*                  Z-ADD     0             ARBPDC
022300070601     C*                  Z-ADD     0             ARBPDC
022400950524     C*  Aggiorno FNARB e leggo record successivo
022500070601     C*                  EXCEPT    BLKDIS
022600070601     C*                  END
022700030725     C*  Chiudo eventuali anomalie su dettaglio segnacolli
022800070601     C*                  EXSR      CLOANM
022900070601     C*    KARB          READE     FNARB70L                               33
023000070601     C*                  ENDdo
023100950613     C*
023200950613     C                   ENDSR
023300950613     C*-----------------------------------------------------*
023400950613     C*  Gestione sinogola distinta
023500950613     C*-----------------------------------------------------*
023600950613     C     ALLOC         BEGSR
023700030728     C                   WRITE     ERR2
023800030728     C                   ADD       1             D59LIN
023900950613     C*
024000950613     C                   ENDSR
024100080507     C*-----------------------------------------------------*
024200080507     C*  Gestione pda
024300080507     C*-----------------------------------------------------*
024400080507     C     srpda         BEGSR
024500080507     C                   WRITE     ERR3
024600080507     C                   ADD       1             D59LIN
024700080507     C*
024800080507     C                   ENDSR
024900080507     C*-----------------------------------------------------*
025000080507     C*  stampa anomalia distinta
025100080507     C*-----------------------------------------------------*
025200080507     C     stampa        BEGSR
025300080507     C*
025400080507     C                   Z-ADD     FVVfgs        PRTFGS            3 0
025500080507     C                   Z-ADD     FVVNFV        PRTNDT            5 0
025600110209     C                   Z-ADD     FVVNFV        PRTNDT3
025700110209     C*                  eval      PRTNDT2 = d59bcd
025800110209     C                   move      d59bcd        PRTNDT2
025900080507     C                   CLEAR                   WLBDA8
026000080507     C                   Z-ADD     FVVDFV        G02INV
026100080507     C                   MOVEL     '3'           G02ERR
026200080507     C                   CALL      'XSRDA8'
026300080507     C                   PARM                    WLBDA8
026400080507     C                   Z-ADD     G02DAT        STCDDC
026500080507     C                   MOVEL     FVVDSF        PRTDSF           35
026600080507     C                   MOVEL     FVVDSF        PRTDSF2
026700080507     C                   MOVEL     CMAT          TPPRE
026800080507     C     FVVFPP        IFEQ      'P'
026900080507     C                   MOVEL     CPOM          TPPRE            10
027000080507     C                   END
027100080507     C*
027200080507     C                   IF        D59LIN = 0 OR D59LIN > 54
027300080507     C                   SETON                                        80
027400080507     C                   END
027500080507     C*
027600080507     c                   if        *in80
027700080507     C                   ADD       1             D59PAG
027800080507     C                   WRITE     TESTA
027900080507     c                   if        d59lsr = 'S'  and d59pag = 1
028000080507     C                   write     testal
028100080507     c                   end
028200080507     C                   WRITE     TESTA1
028300080507     C                   Z-ADD     6             D59LIN
028400080507     c                   setoff                                       80
028500080507     c                   end
028600080507     C*
028700080507     C                   ENDSR
028800980309     C*-----------------------------------------------------*
028900980309     C*  Scrivo evento di eclusione dalla consegna in QTEMP
029000980309     C*-----------------------------------------------------*
029100980309     C     WRTNIC        BEGSR
029200980309     C*
029300980310     C*  Solo se bolla non consegnata
029400980310     C     ARBDCM        IFEQ      0
029500980309     C*  Controllo se esiste evento MIC
029600980318     C                   Z-ADD     ARBAAS        KAAS
029700980318     C                   Z-ADD     ARBLNP        KLNP
029800980318     C                   Z-ADD     ARBNRS        KNRS
029900980318     C                   Z-ADD     ARBNSP        KNSP
030000980318     C     KLBL          CHAIN     FNLBL01L                           34
030100980330     C     *IN34         IFEQ      '0'
030200980318     C     LBLLAP        ANDEQ     LBLLAN
030300980429     C                   Z-ADD     LBLAAP        KAAS
030400980429     C                   Z-ADD     LBLLPP        KLNP
030500980429     C                   Z-ADD     LBLNRP        KNRS
030600980429     C                   Z-ADD     LBLNSP        KNSP
030700980429     C     KARB1         CHAIN     FNARB01L                           34
030800980429     C     ARBCCA        IFEQ      '2'
030900980429     C     ARBCCA        OREQ      '6'
031000980429     C                   SETON                                        34
031100980429     C                   END
031200980429     C     *IN34         IFEQ      '0'
031300980429     C     LBLLAP        ANDEQ     LBLLAN
031400990803     C                   Z-ADD     LBLAAP        KAAS
031500980318     C                   Z-ADD     LBLLPP        KLNP
031600980318     C                   Z-ADD     LBLNRP        KNRS
031700980318     C                   Z-ADD     LBLNSP        KNSP
031800980318     C                   ELSE
031900990803     C                   Z-ADD     LBLAAN        KAAS
032000980430     C                   Z-ADD     LBLLPN        KLNP
032100980430     C                   Z-ADD     LBLNRN        KNRS
032200980430     C                   Z-ADD     LBLNSN        KNSP
032300980318     C                   END
032400980721     C* SE NON HO AGGANCIATO FNLBL IMPOSTO NEI DATI DELLA
032500980721     C* BOLLA SEGUENTE I DATI DELLA SPEDIZIONE
032600980721     C                   ELSE
032700980721     C                   Z-ADD     ARBAAS        LBLAAN
032800980721     C                   Z-ADD     ARBLNP        LBLLPN
032900980721     C                   Z-ADD     ARBNRS        LBLNRN
033000980721     C                   Z-ADD     ARBNSP        LBLNSN
033100980721     C                   END
033200000705      *
033300980309     C                   MOVEL     'MIC'         KCEV
033400990803     C     KEVB          CHAIN     FNEVB04L                           35
033500980309     C     *IN35         IFEQ      '0'
033600980309     C* Se esiste scrivo evento 'NIC' su archivio eventi
033700980309     C* provvisorio in QTEMP
033800031014     C                   CLEAR                   FNEVBPRV
033900980309     C                   MOVEL     'NIC'         EVBCEV
034000980310     C                   Z-ADD     WDAY          EVBDTV
034100980309     C                   TIME                    EVBORV
034200980310     C                   Z-ADD     D59FGS        EVBFLE
034300020916     C                   Z-ADD     DUTCOU        EVBCDU
034400990803     C                   Z-ADD     KAAS          EVBAAS
034500980318     C                   Z-ADD     KLNP          EVBLNP
034600980318     C                   Z-ADD     KNRS          EVBNRS
034700980318     C                   Z-ADD     KNSP          EVBNSP
034800980309     C                   Z-ADD     FVVDFV        EVBDEV
034900980309     C     FVVFPP        IFEQ      'M'
035000031013     C                   MOVEL     0800          EVBHEV
035100980309     C                   ELSE
035200031013     C                   MOVEL     1200          EVBHEV
035300980309     C                   END
035400131104      **
035500131104     C                   clear                   Devb01
035600131104     C                   move      *zeros        §NOTNDC
035700131104     C                   move      *zeros        §NOTPDC
035800131104     C                   move      ARBNDC        §NOTNDC
035900131104     C                   move      ARBPDC        §NOTPDC
036000131104     C                   MOVEL     Devb01        EVBNOT
036100131104      **
036200990803     C                   Z-ADD     0             EVBDTC
036300990803     C                   WRITE     FNEVBPRV
036400980309     C                   END
036500980310     C                   END
036600980430     C                   Z-ADD     LBLAAN        KAAS
036700980430     C                   Z-ADD     LBLLPN        KLNP
036800980430     C                   Z-ADD     LBLNRN        KNRS
036900980430     C                   Z-ADD     LBLNSN        KNSP
037000980430     C     KARB1         CHAIN     FNARB01L                           34
037100980310     C*
037200980310     C                   ENDSR
037300950614     C*-----------------------------------------------------*
037400950614     C*  Chiudo eventuali anomalie su dettaglio segnacolli
037500950614     C*-----------------------------------------------------*
037600950915     C     CLOANM        BEGSR
037700950614     C*  Controllo se bolla con segnacolli
037800950915     C                   Z-ADD     ARBAAS        KAAS
037900950915     C                   Z-ADD     ARBLNP        KLNP
038000950915     C                   Z-ADD     ARBNRS        KNRS
038100950915     C                   Z-ADD     ARBNSP        KNSP
038200950915     C     KART          CHAIN     FNART01L                           34
038300950614     C*  Se non esiste e spedizione figlia mi posiziono su mamma
038400950915     C     *IN34         IFEQ      '1'
038500950915     C     KLBL          CHAIN     FNLBL01L                           34
038600950915     C     *IN32         IFEQ      '0'
038700950915     C     LBLLAP        ANDEQ     LBLLAN
038800950915     C                   Z-ADD     LBLAAP        KAAS
038900950915     C                   Z-ADD     LBLLPP        KLNP
039000950915     C                   Z-ADD     LBLNRP        KNRS
039100950915     C                   Z-ADD     LBLNSP        KNSP
039200950915     C     KART          CHAIN     FNART01L                           34
039300950915     C                   END
039400950915     C                   END
039500030721     C*  Se ho trovato i segnacolli vado a chiudere le anomalie "bolla in
039600030721     c* consegna"
039700950915     C     *IN34         IFEQ      '0'
039800960415     C                   CLEAR                   DSLR33
039900960415     C                   MOVE      '4'           D33FSC
040000960415     C                   MOVE      WDAY          D33DCH
040100960415     C                   MOVEL     'ED'          D33CCH
040200990519     C                   Z-ADD     ARTAAS        KAA4
040300950915     C                   Z-ADD     135           KCAA
040400990519     C     KANM          CHAIN     FNANM01L                           34
040500950915     C     *IN34         DOWEQ     '0'
040600960415     C                   Z-ADD     ANMNR1        D33NRR
040700960415     C                   CALL      'FNLR33R'
040800960415     C                   PARM                    DSLR33
040900990519     C     KANM          READE     FNANM01L                               34
041000950915     C                   END
041100030721     C*  chiudo anomalia "Bolla in consegna su altra distinta"
041200950915     C                   Z-ADD     150           KCAA
041300990519     C     KANM          CHAIN     FNANM01L                           34
041400950915     C     *IN34         DOWEQ     '0'
041500960415     C                   Z-ADD     ANMNR1        D33NRR
041600960415     C                   CALL      'FNLR33R'
041700960415     C                   PARM                    DSLR33
041800990519     C     KANM          READE     FNANM01L                               34
041900950915     C                   END
042000950915     C*
042100950915     C                   END
042200950614     C*
042300950915     C                   ENDSR
042400950524     C*--------------------------------------------------------------*
042500950524     C*  Operazioni iniziali
042600950524     C*--------------------------------------------------------------*
042700950524     C     *INZSR        BEGSR
042800950524     C*
042900020916     c     *dtaara       define    §azute        azuteds
043000020916     c     *dtaara       define    §datiute      ddatiute
043100020916     C                   in(E)     *dtaara
043200020916     C                   IF        %Error  or  RSUT = *blanks
043300020916     C                   call      'TIBS34R'
043400020916     C                   parm                    Tibs34Ds
043500020916     C                   in        *dtaara
043600020916     c                   ENDIF
043700950613     C*
043800950915     C     KLBL          KLIST
043900950915     C                   KFLD                    KAAS
044000950915     C                   KFLD                    KLNP
044100950915     C                   KFLD                    KNRS
044200950915     C                   KFLD                    KNSP
044300950915     C     KART          KLIST
044400950915     C                   KFLD                    KAAS
044500950915     C                   KFLD                    KLNP
044600950915     C                   KFLD                    KNRS
044700950915     C                   KFLD                    KNSP
044800980429     C     KARB1         KLIST
044900980429     C                   KFLD                    KAAS
045000980429     C                   KFLD                    KLNP
045100980429     C                   KFLD                    KNRS
045200980429     C                   KFLD                    KNSP
045300950915     C     KANM          KLIST
045400990519     C                   KFLD                    KAA4
045500950915     C                   KFLD                    KLNP
045600950915     C                   KFLD                    KNRS
045700950915     C                   KFLD                    KNSP
045800950915     C                   KFLD                    KCAA
045900980310     C     KEVB          KLIST
046000990803     C                   KFLD                    KAAS
046100980310     C                   KFLD                    KLNP
046200980310     C                   KFLD                    KNRS
046300980310     C                   KFLD                    KNSP
046400980310     C                   KFLD                    KCEV
046500950613     C     KBRV          KLIST
046600950613     C                   KFLD                    KNPG
046700000114     C                   KFLD                    KNFV1
046800950613     C                   KFLD                    KFGS
046900950613     C     KFVV1         KLIST
047000950613     C                   KFLD                    KNPG
047100950613     C                   KFLD                    KNFV
047200950613     C                   KFLD                    KFGS
047300950613     C     KFVV2         KLIST
047400950613     C                   KFLD                    KNPG
047500950613     C                   KFLD                    KDFV
047600950613     C                   KFLD                    KFGS
047700960702     C     KARB          KLIST
047800020916     C                   KFLD                    KIFP
047900020916     C                   KFLD                    KNDC
048000960702     C                   KFLD                    KDDC
048100080507     C     KDSt9         KLIST
048200080507     C                   KFLD                    kFGS
048300080507     C                   KFLD                    KNPG
048400080507     C                   KFLD                    knfv1
048500090223     C     Kwf           KLIST
048600090223     C                   KFLD                    kFGS
048700090223     C                   KFLD                    knfv1
048800080507     C     KDSf          KLIST
048900080507     C                   KFLD                    KNPG
049000080507     C                   KFLD                    knfv1
049100080507     C                   KFLD                    kFGS
049200080507     C                   KFLD                    dsftrd
049300080507     c                   movel     'PDC'         dsftrd
049400021204     C     Kapd          KLIST
049500021204     C                   KFLD                    apdtip
049600110518     C                   KFLD                    £dstpdr
049700021204     c                   movel     'A'           apdtip
049800950524     C*
049900990519     C     *LIKE         DEFINE    ANMAAS        KAA4
050000950915     C     *LIKE         DEFINE    ANMCAA        KCAA
050100980310     C     *LIKE         DEFINE    EVBCEV        KCEV
050200020916     C     *LIKE         DEFINE    ARBIFP        KIFP
050300020916     C     *LIKE         DEFINE    ARBAAS        KAAS
050400950524     C     *LIKE         DEFINE    ARBLNP        KLNP
050500950524     C     *LIKE         DEFINE    ARBNRS        KNRS
050600950524     C     *LIKE         DEFINE    ARBNSP        KNSP
050700950613     C     *LIKE         DEFINE    FVVNPG        KNPG
050800950613     C     *LIKE         DEFINE    FVVNFV        KNFV
050900000114     C     *LIKE         DEFINE    BRVNFV        KNFV1
051000950613     C     *LIKE         DEFINE    FVVFGS        KFGS
051100950613     C     *LIKE         DEFINE    FVVDFV        KDFV
051200950613     C     *LIKE         DEFINE    ARBNDC        KNDC
051300960702     C     *LIKE         DEFINE    ARBDDC        KDDC
051400950524     C*
051500030728     C                   SETON                                        80
051600950613     C                   TIME                    TIMES             6 0
051700950613     C                   TIME                    WHDAT            12 0
051800950613     C                   MOVE      WHDAT         WOGGI             6 0
051900960418     C*
052000960418     C                   TIME                    WDATE            14 0
052100960418     C                   CLEAR                   WLBDA8
052200960418     C                   MOVE      WDATE         G02DAT
052300960418     C                   CALL      'XSRDA8'
052400960418     C                   PARM                    WLBDA8
052500960418     C                   MOVE      G02INV        WDAY              8 0
052600950613     C*
052700030728     C                   OPEN      FNLRF7P
052800950613     C*
052900950524     C                   ENDSR
053000950524     O*--------------------------------------------------------------*
053100950524     O*  DEFINISCO EXCPT
053200950524     O*--------------------------------------------------------------*
053300070601     O*NARB000  E            BLKDIS
053400070601     O*                      ARBNSS
053500070601     O*                      ARBIFP
053600070601     O*                      ARBNDC
053700070601     O*                      ARBDDC
053800070601     O*                      ARBSTP
053900070601     O*                      ARBPDC
054000950613     OFNFVV001  E            AGGFL1
054100950613     O                       FVVFCF
054200950613     OFNFVV000  E            AGGFL2
054300950613     O                       FVVFCF
