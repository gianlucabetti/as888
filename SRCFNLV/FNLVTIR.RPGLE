000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200170719     H* FLBVTIR *-----------------------------------------------------*
000300170719     H*  Aggiorna instradam. bolle sosp a cambio cappario
000400930126     H*---------------------------------------------------------------*
000500170719     Ffnlvtid   CF   E             WORKSTN
000600040611     FAZORG01L  IF   E           K DISK
000700080319     Ffnfvv01l  IF   E           K DISK
000800170719     Ffnblp45l  iF   E           K DISK
000900170719     Ffnblp01l  iF   E           K DISK    rename(fnblp000:fnblp001)
001000170719     Ffnlbl01l  iF   E           K DISK
001100170719     FQSYSPRT   O    F  132        PRINTER
001200080319     d*-----------------
001300170719     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001400080319     d*-----------------
001500911010     D WLBDAT          DS
001600941221     D  G02DAT                 1      8  0
001700941221     D  G02INV                 9     16  0
001800941221     D  G02ERR                17     17
001900941221     D  G02TGI                18     22  0
002000080319     d
002100040604     D CNCR80        E DS
002200040604     D KPJBA         E DS
002300030722     D UT§DSE0F      E DS
002400170719     D* DS PER TRUL90R - RICHIESTA STAMPANTI
002500170719     D TRUL90DS      E DS
002600170719     D DBLP          E DS                  INZ
002700170719     D DTASV         E DS                  INZ
002800170719     D og143         E DS                  INZ
002900080319     d
003000040611     d fnlv55DS      e ds
003100040614     D*
003200080319     d Dataiso         s               d   datfmt(*iso)
003300080319     d Dataeur         s               d   datfmt(*eur)
003400000000     C*---------------------------------------------------------------*
003500911010     C* INDICATORI
003600911010     C*---------------------------------------------------------------*
003700951115     C*  28    - EMETTE CAMPO MESSAGGI
003800040607     C*  40    - ERRORE
003900911010     C*  90    - ERRORE GENERICO
004000911010     C*---------------------------------------------------------------*
004100000000     C     *ENTRY        PLIST
004200000000     C                   PARM                    KPJBA
004300941221     C*
004400000000     C                   Z-ADD     1             CODUT
004500911010     C                   CALL      'X§PARUT'
004600030722     C                   PARM                    UT§DSE0F
004700920901     C                   MOVEL     REC80         CNCR80
004800941221     C                   MOVEL     RAGUT         RSUT
004900170719
005000170719     C                   CLEAR                   trul90DS
005100170719     C                   MOVEL     'S'           d90RSE
005200170719     C                   MOVEL     'S'           d90RSB
005300170719     C                   CALL      'TRUL90R'
005400170719     C                   PARM                    KPJBA
005500170719     C                   PARM                    trul90DS
005600170719      *
005700170719      * F3 - FINE
005800170719     C     d90F3         IFEQ      '1'
005900170719     C                   GOTO      FINE
006000170719     C                   ENDIF
006100900511     C*---------------------------------------------------------------*
006200911008     C                   EXSR      PULIZ
006300971223     C*
006400921123     C     FOR01         TAG
006500911008     C*
006600170719     C                   EXFMT     lvtiD01
006700040607     C*
006800040607     C                   SETOFF                                       409028
006900080319     C                   SETOFF                                       414243
007000030722     C** F3 - FINE LAVORO
007100040604     C   KC              GOTO      FINE
007200921117     C*
007300040611     C* CONTROLLO
007400911008     C                   EXSR      CONTR1
007500930127     C* SE ERRORE
007600040607     c  nkf
007700040607     Cor 90              GOTO      FOR01
007800040528     c
007900030722     C* F6  - ELABORAZIONE
008000040607     c                   if        *inkf
008100170719     c                   exsr      ELAb
008200170719     c                   goto      for01
008300040604     c                   endif
008400040604     c*
008500000000     C     FINE          TAG
008600040611     c                   clear                   fnlv55ds
008700040611     c                   eval      d55tla='C'
008800040611     C                   CALL      'FNLV55R'
008900040611     C                   PARM                    fnlv55ds
009000960202     C*
009100000000     C                   SETON                                        LR
009200941216     C**************************************************************************
009300941216     C*    PULIZIA CAMPI
009400941216     C**************************************************************************
009500911008     C     PULIZ         BEGSR
009600941216     C*
009700040607     C                   TIME                    w0140            14 0
009800080319     C                   MOVE      w0140         videla
009900080328     c                   movel     videla        dataeur
010000080328     c                   movel     dataeur       dataiso
010100080328     c                   movel     dataiso       amgudate          8 0
010200040528     c
010300040604     C                   TIME                    UTIME             6 0
010400170719     c                   clear                   vidlnp
010500170719     c                   clear                   vidlna
010600170719     c                   clear                   deslnp
010700170719     c                   clear                   deslna
010800170719     c                   clear                   out1
010900170719     c                   clear                   out2
011000040611     c
011100080319     c
011200170719     C     Kblp          KLIST
011300170719     C                   KFLD                    blPAAS
011400170719     C                   KFLD                    bLPLNP
011500170719     C                   KFLD                    bLPNRS
011600170719     C                   KFLD                    bLPNSP
011700040607     c*
011800911008     C                   ENDSR
011900960205     C**************************************************************************
012000040607     C*    CONTROLLI data arrivo  colli
012100960205     C**************************************************************************
012200960205     C     CONTR1        BEGSR
012300040607     C*
012400170719     c                   clear                   out1
012500170719     c                   clear                   out2
012600080328     c
012700080328     c* se c'e' la data controllo prima del p.o.
012800080328     c                   clear                   comela
012900080328     c                   if        videla>0
013000080328     C                   Z-ADD     videla        G02DAT
013100080328     C                   CLEAR                   G02ERR
013200080328     C                   CALL      'XSRDA8'
013300080328     C                   PARM                    WLBDAT
013400080328     C*
013500080328     C* Data errata
013600080328    1C     G02ERR        IFEQ      '1'
013700080328     C                   MOVEL     msg(1)        VIDMSG
013800080328     C                   SETON                                        419028
013900080328     c                   goto      endct1
014000080328     c                   else
014100080328     c                   z-add     g02dat        videla
014200080328     c                   z-add     g02inv        comela            8 0
014300080328    1C                   ENDIF
014400080328    1C                   ENDIF
014500040611     c* controllo p.o.
014600170719     c                   clear                   deslnp
014700170719    1c                   if        vidlnp<>0
014800080328     c
014900040611     c* Deve essere un p.o. del terminal di partenza del p.o. gestione
015000040611     c                   clear                   fnlv55ds
015100040611     C                   MOVEL     'P'           D55TPT
015200170719     C                   MOVEL     vidlnp        D55LIN
015300080328     c                   if        comela>0
015400080328     C                   MOVEL     comela        D55DRF
015500080328     c                   else
015600080328     C                   MOVEL     amgudate      D55DRF
015700080328     c                   endif
015800040611     C                   CALL      'FNLV55R'
015900040611     C                   PARM                    fnlv55ds
016000040611     c*
016100170719    3c                   if        d55err<>*blanks
016200040611     C                   MOVEL     msg(2)        VIDMSG
016300040611     C                   SETON                                        409028
016400080319     c                   goto      endct1
016500080319    3c                   endif
016600080328     c
016700080319     c* decodifico la filiale
016800170719     c     vidlnp        chain     azorg01l
016900080319     c                   if        not %found(azorg01l)
017000170719     C                   MOVEL     msg(2)        VIDMSG
017100080319     C                   SETON                                        409028
017200080319     c                   goto      endct1
017300080319     c                   else
017400170719     c                   movel     orgdes        deslnp
017500080319     c                   endif
017600080319     c
017700040611    1c                   endif
017800170719
017900170719
018000170719    1c                   if        vidlna<>0
018100170719     c
018200170719     c* Deve essere un p.o. del terminal di partenza del p.o. gestione
018300170719     c                   clear                   fnlv55ds
018400170719     C                   MOVEL     'P'           D55TPT
018500170719     C                   MOVEL     vidlna        D55LIN
018600170719     c                   if        comela>0
018700170719     C                   MOVEL     comela        D55DRF
018800170719     c                   else
018900170719     C                   MOVEL     amgudate      D55DRF
019000170719     c                   endif
019100170719     C                   CALL      'FNLV55R'
019200170719     C                   PARM                    fnlv55ds
019300170719     c*
019400170719    3c                   if        d55err<>*blanks
019500170719     C                   MOVEL     msg(2)        VIDMSG
019600170719     C                   SETON                                        429028
019700170719     c                   goto      endct1
019800170719    3c                   endif
019900170719     c
020000170719     c* decodifico la filiale
020100170719     c     vidlna        chain     azorg01l
020200170719     c                   if        not %found(azorg01l)
020300170719     C                   MOVEL     msg(2)        VIDMSG
020400170719     C                   SETON                                        429028
020500170719     c                   goto      endct1
020600170719     c                   else
020700170719     c                   movel     orgdes        deslna
020800170719     c                   endif
020900170719     c
021000170719    1c                   endif
021100080319     c
021200040611     c
021300040611     C     endct1        ENDSR
021400170719     c*----------------------------------------------------------------------
021500170719     c     ELAb          BEGSR
021600170719     C
021700170719     C                   except    testa
021800170719     c                   clear                   errori
021900170719     c                   clear                   aggio
022000170719     c                   clear                   bolle
022100170719
022200170719     c     *loval        setll     fnblp45l
022300170719    1c                   do        *hival
022400170719     c                   read      fnblp45l
022500170719    2c                   if        %eof(fnblp45l)
022600170719     c                   leave
022700170719    2c                   endif
022800170719     c* scarto se figlia
022900170719     c     kblp          setll     fnlbl01l
023000170719     c                   if        %equal(fnlbl01l)
023100170719     c                   iter
023200170719     c                   endif
023300170719     c*
023400170719     c* Scarto lna export e recupero  e disk B  e spuntate in entrata
023500170904     c* e i fermi deposito
023600170719     c                   if        blpctm ='4 ' or blpctm='4F'
023700170719     c                             or blpnrs>0 or blpdse>0
023800170904     c                             or blpffd='S'
023900170719     c                   iter
024000170719     c                   endif
024100170904     c
024200170904     c* Escludo i preparati non confermati
024300170904     c                   move      blpksc        w004a             4
024400170904     c                   if        w004a='8888'
024500170904     c                   iter
024600170904     c                   endif
024700170719
024800170719     c* scarto se selezionate lnp o lna e entrame
024900170719     c                   if        vidlnp>0 and vidlnp<>blplnp
025000170719     c                   iter
025100170719     c                   endif
025200170719     c                   if        vidlna>0 and vidlna<>blplna
025300170719     c                   iter
025400170719     c                   endif
025500170719
025600170719     c     blplna        chain     azorg01l
025700170719     c                   if        %found(azorg01l)
025800170719     c                   eval      og143=orgde3
025900170719     c                   else
026000170719     c                   clear                   og143
026100170719     c                   endif
026200170719     c                   if        §ogntw='EEX' or §ogntw='DPD' or
026300170719     c                             §ogntw='FED'
026400170719     c                   iter
026500170719     c                   endif
026600170719
026700170719     c* data immissione < della data nuovo cappario --> elaboro
026800170719    2c                   if        blpdim < comela
026900170719     c*
027000170719     c* richiamo fnls01r
027100170719     c                   movel     blplna        savlna            3 0
027200170719     C                   MOVEL     BLPAAS        §LPAAS
027300170719     C                   MOVEL     BLPLNP        §LPLNP
027400170719     C                   MOVEL     BLPNRS        §LPNRS
027500170719     C                   MOVEL     BLPNSP        §LPNSP
027600170719     C                   MOVEL     'N'           §LPFPR
027700170719     C*                  MOVEL     PARAM4        KPJBU
027800170719     C                   CALL      'FNLS01R'
027900170719     C                   PARM                    KPJBA
028000170719     C                   PARM                    DBLP
028100170719     C                   PARM                    DTASV
028200170719     C                   PARM                    trul90ds
028300170719     C                   PARM                    blptfp
028400170719     c
028500170719    3c                   if        §lpfpr>'0'
028600170719     c                   except    err
028700170719     c                   add       1             errori            7 0
028800170719   x3c                   else
028900170719     c     kblp          chain     fnblp01l
029000170719    4c                   if        %found(fnblp01l) and
029100170719     c                             savlna<>blplna
029200170719     c                   except    bol
029300170719     c                   add       1             aggio             7 0
029400170719    4c                   endif
029500170719    3c                   endif
029600170719
029700170719     c                   add       1             bolle             7 0
029800170719     C*
029900170719    2c                   endif
030000170719    1c                   enddo
030100170719     c*
030200170719     c* preparo msg di output
030300170719     c                   if        bolle>0
030400170719     c                   eval      out1='Bolle ELABORATE :  '+
030500170719     c                             %editc(bolle:'2')
030600170719     c                   eval      out2='di cui bolle Aggiornate  ' +
030700170719     c                             %editc(aggio:'2') + '  con Errore  ' +
030800170719     c                             %editc(errori:'2') +'.  Vedi la stampa.'
030900170719     c                   else
031000170719     c                   eval      out1='NESSUNA  BOLLA  ELABORATA'
031100170719     c                   endif
031200170719     c
031300170719     c                   except    tot
031400170719     c                   ENDSR
031500170719     c
031600170719     Oqsysprt   e            TESTA       1
031700170719     o                                        +  25 '** AGGIORNA INSTRADAMENTO '
031800170719     o                                        +   0 'BOLLE IN SOSPENSIONE PE'
031900170719     o                                        +   0 'R CAMBIO CAPPARIO **'
032000170719     Oqsysprt   e            err         1
032100170719     O                       blpaas           +   2
032200170719     O                       blplnp           +   2
032300170719     O                       blpnrs        z  +   2
032400170719     O                       blpnsp        z  +   2
032500170719     o                       blpdim           +   2
032600170720     o                       blplod           +   2
032700170719     o                                        +   2 'ERRORE'
032800170719     Oqsysprt   e            bol         1
032900170719     O                       blpaas           +   2
033000170719     O                       blplnp           +   2
033100170719     O                       blpnrs        z  +   2
033200170719     O                       blpnsp        z  +   2
033300170719     o                       blpdim           +   2
033400170719     o                                        +   2 'OLD lna'
033500170719     o                       savlna           +   2
033600170719     o                                        +   2 'NEW lna'
033700170719     o                       blplna           +   2
033800170719
033900170719     Oqsysprt   e            tot         1
034000170719     o                                              'Bolle Elaborate:'
034100170719     O                       bolle         2  +   2
034200170719     o                                        +   2 'di cui aggiorante'
034300170719     O                       aggio         2  +   2
034400170719     o                                        +   2 'di cui con errore'
034500170719     O                       errori        2  +   2
034600000606**  MSG
034700080319Data elaborazione errata                                                      1
034800080319Filiale inesistente
