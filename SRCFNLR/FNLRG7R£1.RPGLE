000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200131120     H* FNLRG7R *----------------------------------------------------*
000300070403     H* - Interrogazione Abilitazioni bolla
000400000000     H*--------------------------------------------------------------*
000500070326     FFiar401L  IF   E           K DISK
000600070316     FFNARB01L  IF   E           K DISK
000700070427     FFNEVB22L  IF   E           K DISK
000800070327     Ffiarn12l  iF a E           K DISK
000900070327     Ftabel00f  IF   E           K DISK
001000131120     FFNLRG7D   CF   E             WORKSTN usropn
001100131120     F                                     SFILE(LRG7s04:NRR)
001200030722     F**
001300070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001400070330     D TES             S             45    DIM(1) CTDATA PERRCD(1)
001500070426     D ABI             S              1    DIM(15)
001600070313     d
001700051123      * ds di passaggio dati
001800131120     D fnLRG7ds      E DS
001900070326     D  BLOV                  19     27    DIM(9)
002000070313     d
002100070326     D DSBL4L        E DS
002200070314     D KPJBA         E DS
002300070327     D DS3A          E DS
002400070313     d
002500070316     D DSRECARB      E DS                  extname(fnarb00f)
002600070326     D DSRECAR4L     E DS                  extname(fiar400f)
002700070327     D DSparARB      E DS                  extname(fnarb00f) prefix(par:3)
002800070327     D DSparAR4L     E DS                  extname(fiar400f) prefix(pa4:3)
002900070316     d
003000070326     D kTRC            S                   LIKE(ar4trc) inz('L')
003100070327     D codut           S                   LIKE(tblkut) inz(1)
003200070327     D cod             S                   LIKE(tblcod) inz('3A')
003300070327     D key             S                   LIKE(tblkey) inz
003400070327     d KCDN            s                   like(arncdn) inz('BGE')
003500131120     d Lung            s              2  0
003600070327     d Dataeur         s               d   datfmt(*eur)
003700070327     D* CAMPO PER DSPATR "UL"
003800070327     D hi              C                   CONST(X'22')
003900900518     C***************************************************************
004000000000     C     *ENTRY        PLIST
004100131120     C                   PARM                    fnLRG7ds
004200070327     C                   PARM                    dspararb
004300070327     C                   PARM                    dsparar4L
004400940223     C*---------------------------------------------------------------*
004500131120     C* TIPO LANCIO, iLRG7TLA "C" -           CHIUSO CON LR
004600131120     C* TIPO LANCIO, iLRG7TLA "L" - ELABORO E CHIUDO CON LR
004700131120     C* TIPO LANCIO, iLRG7TLA " " - ELABORO E CHIUDO IN RETRN
004800941216     C*
004900131120    1C     iLRG7TLA      IFNE      'C'
005000131120     c                   clear                   oLRG7err
005100131120     c                   clear                   oLRG7msg
005200131120     c                   clear                   oLRG7vssin
005300131120     c                   clear                   oLRG7ABI
005400070426     c                   clear                   ABI
005500070412     c
005600051123     c
005700070313     C* SE NON PASSATo record spedizione, lo chaino
005800070327     C                   IF        (%parms>=2 and parnsp=0) or
005900070315     c                              %parms=1
006000070313     c
006100070313     c     karb          chain     fnarb01l
006200070313     c                   if        not %found(fnarb01l)
006300070313     c* restituisco errore al chiamante ed esco
006400131120     c                   eval      oLRG7msg=msg(1)
006500131120     c                   eval      oLRG7err='E'
006600070313     c                   goto      fine
006700070313     c                   endif
006800070327     c                   else
006900070327     c                   movel     dspararb      dsrecarb
007000070313     c                   endif
007100070326     c
007200070327     C                   IF        (%parms=3 and pa4nsp=0) or
007300070326     c                              %parms<3
007400070326     c
007500070326     c     kar4          chain     fiar401l
007600070326     c                   if        not %found(fiar401l)
007700070402     c* Se non trovato clearo il campo ar4not
007800070402     c                   clear                   ar4not
007900070402     c                   clear                   ar4aas
008000070402     c                   clear                   ar4lnp
008100070402     c                   clear                   ar4nrs
008200070402     c                   clear                   ar4nsp
008300070402     c                   clear                   ar4trc
008400070326     c                   endif
008500070327     c                   else
008600070327     c                   movel     dsparar4L     dsrecar4L
008700070326     c                   endif
008800070326     c* riempi la DSL4L
008900070326     c                   movel     ar4not        dsbl4l
009000070326     c
009100070326     c* se passato '?' visulizzo finestra con situazione ANALITICa
009200131120     C                   IF        ILRG7VSDET='?'
009300070419     c
009400131120     c                   open      fnLRG7d
009500070419     c
009600131120     c****               write     LRG7d02
009700070326     c                   exsr      VISUALDETT
009800070327     c* Emetto videata
009900070327     c
010000070403     c                   setoff                                       kl
010100131120     c**                 dow       not *inkl
010200131120     c                   exfmt     LRG7w01
010300070403     c
010400070403     c* Se messo '?' su interrogazione storico sospensione, emetto altra vid
010500070403     c                   if        v1csto='?'
010600070403     c                   exsr      STORICOnote
010700070403     c                   clear                   v1csto
010800070403     c                   setoff                                       kl
010900070403     c                   endif
011000070403     c
011100131120     c**                 enddo
011200131120     C                   WRITE     frcDta
011300070327     c
011400070326     c                   else
011500070326     c*  altrimenti restituisco riga SINTETICA dei dati
011600070326     c                   exsr      VISUALSIN
011700070326     c                   endif
011800070314     c
011900070314     c     fine          TAG
012000070314     c
012100070314     c                   endif
012200051123     c
012300131120     C     ILRG7tla      IFNE      ' '
012400131120     C     ILRG7VSDET    oreq      '?'
012500051123     C                   SETON                                        LR
012600051123     C                   ELSE
012700051123     C                   SETON                                        RT
012800051123     C                   ENDIF
012900991206     C**************************************************************************
013000051123     C     *INZSR        BEGSR
013100051123     c
013200070313     c     kARB          Klist
013300131120     c                   Kfld                    iLRG7Aas
013400131120     c                   Kfld                    iLRG7Lnp
013500131120     c                   Kfld                    iLRG7nrs
013600131120     c                   Kfld                    iLRG7nsp
013700070326     c     kAR4          Klist
013800131120     c                   Kfld                    iLRG7Aas
013900131120     c                   Kfld                    iLRG7Lnp
014000131120     c                   Kfld                    iLRG7nrs
014100131120     c                   Kfld                    iLRG7nsp
014200070326     c                   Kfld                    KTRC
014300051123     c
014400070327     C     KTAB1         KLIST
014500070327     C                   KFLD                    CODUT
014600070327     C                   KFLD                    COD
014700070327     C                   KFLD                    KEY
014800070327     C     KARN          KLIST
014900070327     C                   KFLD                    arbAAS
015000070327     C                   KFLD                    arbLNP
015100070327     C                   KFLD                    arbNRS
015200070327     C                   KFLD                    arbNSP
015300070327     C                   KFLD                    kcdn
015400070313     c                   endsr
015500070313     C**************************************************************************
015600070326     C     VISUALSIN     BEGSR
015700070327     c                   clear                   wrigaSIN        100
015800070327     c                   clear                   wcorta            1
015900070327     c                   exsr      DECODI
016000070327     c*
016100131120     c* Verifico se la decodifica lunga c'e' stata entro i caratteri previsti
016200070327     c     ' '           checkr    wrigaSIN      posiz             3 0    34
016300070327     c
016400070327     c                   select
016500131120     c                   when      (*in34 and posiz<=ilrg7Lung and
016600070327     c                             %subst(WrigaSIN:1:1)<>'-')
016700131120     c                   eval      oLRG7VSSIN=wrigasin
016800070327     c
016900131120     c                   when      (*in34 and posiz<=(ilrg7lung+1) and
017000070327     c                             %subst(WrigaSIN:1:1)='-')
017100131120     c                   eval      oLRG7VSSIN=%subst(wrigaSIN:2:ilrg7Lung)
017200070327     c                   other
017300070327     c
017400070327     c* Versione più ridotta perchè non ci sta
017500070327     c                   clear                   wrigaSIN
017600070427     c                   clear                   ABI
017700070327     c                   eval      wcorta='S'
017800070327     c                   exsr      DECODI
017900131120
018000131125     c                   if        %subst(wrigaSIN:1:1)='/'
018100131120     c                   eval      oLRG7VSSIN=%subst(wrigaSIN:2:ilrg7lung)
018200070327     c                   else
018300131120     c                   eval      oLRG7VSSIN=wrigasin
018400070327     c                   endif
018500131120
018600070327     c                   ENDSL
018700070326     c
018800131120     c                   movea     ABI           oLRG7abi
018900070326     c*
019000070326     c                   endsr
019100070327     C**************************************************************************
019200070327     c     DECODI        BEGSR
019300131120     c* Abilitazione merce
019400131120     c* Escludo se non da visualizzare
019500131120     c     'M'           lookup    BLOV                                   35
019600131120    1c                   if        not *in35
019700131120     c                   select
019800131120     c                   when      Arbama=0
019900131120     c                   eval      wrigaSIN='Merce:NO'
020000131120     c                   when      Arbama=1
020100131120     c                   eval      wrigaSIN='Merce:SI Part'
020200131120     c                   when      Arbama=2
020300131120     c                   eval      wrigaSIN='Merce:SI '
020400131120     c                   when      Arbama=3
020500131120     c                   eval      wrigaSIN='Merce:SI Forz'
020600131120     c                   endsl
020700131120     c                   endif
020800131120
020900070327     c* Blocco Generico
021000070327     c* Escludo se non da visualizzare
021100070327     c     'O'           lookup    BLOV                                   35
021200070327    1c                   if        not *in35 and §B4LBGE='S'
021300070426     c*
021400070426     c                   movel     §b4lbge       abi(1)
021500070327    2c                   if        wcorta=' '
021600131120
021700131120    3c                   if        wrigaSIN=*blanks
021800131120     c                   eval      wrigaSIN='SospensCONS'
021900131120   x3c                   else
022000131120     c                   eval      wrigaSIN=%trim(wrigaSIN)+
022100131125     c                                      '/SospensCONS'
022200131120    3c                   endif
022300131120     c
022400131120   x2c                   else
022500131120
022600131120    3c                   if        wrigaSIN=*blanks
022700070327     c                   eval      wrigaSIN='SospCONS'
022800131120   x3c                   else
022900131120     c                   eval      wrigaSIN=%trim(wrigaSIN)+
023000131125     c                                      '/SospCONS'
023100131120    3c                   endif
023200070327    2c                   endif
023300070327    1c                   endif
023400070327     c* Blocco ASSEGNATO
023500070327     c* Escludo se non da visualizzare
023600070327     c     '9'           lookup    BLOV                                   35
023700070327    1c                   if        not *in35 and ARBACA=' '
023800070426     c                   movel     'S'           abi(2)
023900070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
024000131125     c                             '/ASSEGNATO'
024100070327    1c                   endif
024200070327     c* Blocco C/Assegno
024300070327     c* Escludo se non da visualizzare
024400070327     c     'C'           lookup    BLOV                                   35
024500070430    1c*****              if        not *in35 and (ARBACC=' ' or §B4LTCA<>' ')
024600070430    1c                   if        not *in35 and (ARBACC=' ')
024700070426     c                   movel     §b4ltca       abi(3)
024800070327    2c                   if        wcorta=' '
024900070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
025000131125     c                             '/C/ASSEGNO'
025100070327     c                   else
025200070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
025300131125     c                             '/C/ASS'
025400070327    2c                   endif
025500070327    1c                   endif
025600070426     c* Blocco Supermercati/Appuntamento
025700070327     c* Escludo se non da visualizz0re
025800070327     c     'S'           lookup    BLOV                                   34
025900070327     c     'A'           lookup    BLOV                                   35
026000070327    1c                   if        §B4Lbas<>' '
026100070327    2c                   if        not *in34 and (ARBTC1='S' or arbtc2='S')
026200070426     c                   movel     §b4lbas       abi(4)
026300070327    3c                   if        wcorta=' '
026400070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
026500131125     c                             '/SUPERMERCATO'
026600070327     c                   else
026700070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
026800131125     c                             '/SUPERM'
026900070327    3c                   endif
027000070327    2c                   endif
027100070327    2c                   if        not *in35 and (ARBTC1='A' or arbtc2='A')
027200070504     c                   movel     §b4lbas       abi(5)
027300070327    3c                   if        wcorta=' '
027400070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
027500131125     c                             '/APPUNTAMENTO'
027600070327     c                   else
027700070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
027800131125     c                             '/APPUNT'
027900070327    3c                   endif
028000070327    2c                   endif
028100070327    1c                   endif
028200070327     c* Fermo Deposito
028300070327     c* Escludo se non da visualizzare
028400070327     c     'F'           lookup    BLOV                                   35
028500070327    1c                   if        not *in35 and §B4LTFD<>' ' and arbffd='S'
028600070504     c                   movel     §b4ltfd       abi(6)
028700070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
028800131125     c                             '/FDEPOSITO'
028900070327    1c                   endif
029000070327     c* Blocco consegna
029100070327     c* Escludo se non da visualizzare
029200070327     c     'L'           lookup    BLOV                                   35
029300070427     c                   if        not *in35
029400070427     c
029500070427     c* Lasciato avviso: solo se da telefonare:lo  vedo in EVB2
029600070427    1c                   if        arbfbc='A'
029700070427     c     karb          setgt     fnevb22l
029800070427     c     karb          readpe    fnevb22l
029900070427     c
030000070427     c                   if        not %eof(fnevb22l) and (evb2tel='S'
030100070427     c                             or  evb2tel='T' or evb2tel='N')
030200070427     c
030300070504     c                   movel     evb2tel       abi(7)
030400070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
030500131125     c                             '/LascAVVISO'
030600070327    2c                   endif
030700070427    2c                   endif
030800070427     c
030900070327    2c                   if        arbfbc='G'
031000070504     c                   movel     arbfbc        abi(8)
031100070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
031200131125     c                             '/GIACENTE'
031300070327    2c                   endif
031400070327    2c                   if        arbfbc='B'
031500070504     c                   movel     arbfbc        abi(8)
031600070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
031700131125     c                             '/RIMPALLO'
031800070327    2c                   endif
031900070427    1c                   endif
032000070327     c                   ENDSR
032100070326     C**************************************************************************
032200070326     C     VISUALDETT    BEGSR
032300131120     c                   clear                   LRG7w01
032400070327     c                   movea     blov          w010a            10
032500070330     C                   MOVE      tes(1)        V1CTES
032600070327     c
032700070327     c                   eval      v1clnp=arblnp
032800070327     c                   eval      v1cnrs=arbnrs
032900070327     c                   eval      v1cnsp=arbnsp
033000070403     c                   clear                   v1csto
033100070327     c* Data Spedizione
033200070327     c                   movel     arbaas        w0080
033300070327     c                   move      arbmgs        w0080             8 0
033400070327     c     *iso          movel     w0080         dataeur
033500070327     c                   movel     dataeur       v1cdsp
033600070327     c* codice bolla
033700070327     C                   MOVEL(P)  ARBCBO        KEY
033800070327     C     KTAB1         CHAIN     TABEL                              31
033900070327     C  n31              MOVEL     TBLUNI        DS3A
034000070327     C   31              clear                   DS3A
034100070327     c                   movel     §3atb1        flgtb1            1
034200070327     c
034300070327     c* Blocco Generico
034400070327     c* Escludo se non da visualizzare
034500070327     c     'O'           lookup    BLOV                                   35
034600070403    1c                   if        not *in35
034700070403     c
034800070403    2c                   if        §B4LBGE='S'
034900070327     c                   eval      v1dbge='SI'
035000070327     c                   seton                                        20
035100070403     c                   movel     hi            ATRbge
035200070403    2c                   endif
035300131120     c
035400131120     c* Merce
035500131120     c                   setoff                                       27
035600131120     c                   clear                   v1dama
035700131120     c     'M'           lookup    BLOV                                   35
035800131120    1c                   if        not *in35
035900131120     c                   seton                                        27
036000131120     c                   select
036100131120     c                   when      Arbama=0
036200131120     c                   eval      v1dama='NO'
036300131120     c                   when      Arbama=1
036400131120     c                   eval      v1dama  ='SI Part'
036500131120     c                   when      Arbama=2
036600131120     c                   eval      v1dama  ='SI '
036700131120     c                   when      Arbama=3
036800131120     c                   eval      v1dama  ='SI Forz'
036900131120     c                   endsl
037000131120     c                   endif
037100070403     c
037200070327     c* chain fiarn ultimo record motivo sospensione generica
037300070327     c     karn          chain     fiarn12l
037400070327    2c                   if        %found(fiarn12l)
037500070327     c                   movel     arnnob        v1cnob1
037600070327     c                   move      arnnob        v1cnob2
037700070403    3c                   if        not *in20
037800070403     c                   seton                                        20
037900070403     c                   eval      v1dbge='NO'
038000070403    3c                   endif
038100070403    2c                   endif
038200070327    1c                   endif
038300070327     c* Blocco ASSEGNATO
038400070327     c* Escludo se non da visualizzare
038500070327     c     '9'           lookup    BLOV                                   35
038600070327    1c                   if        not *in35
038700070327    2c                   if        ARBACA=' '
038800070327     c                   seton                                        21
038900070327     c                   eval      v1dass='Da Abilitare'
039000070327     c                   movel     hi            ATRASS
039100070327     c                   else
039200070327     c* Se bolla in assegnato dico se C/A già abilitato
039300070327    3c                   if        flgtb1='A' or §3atb2<>*blanks
039400070327     c                   seton                                        21
039500070327     c                   eval      v1dass='ABILITATO   '
039600070327    3C                   ENDIF
039700070327    2C                   ENDIF
039800070327    1C                   ENDIF
039900070327     c* Blocco C/Assegno
040000070327     c* Escludo se non da visualizzare
040100070327     c     'C'           lookup    BLOV                                   35
040200070327    1c                   if        not *in35 and §3afca<>0
040300070327     c                   seton                                        22
040400070327    2c                   if        ARBACC='S'
040500070327     c                   eval      v1dCAS='ABILITATO    '
040600070327     c                   else
040700070327     c                   movel     §b4ltca       wtel              1
040800070327     c                   exsr      DECODITEL
040900070327     c                   movel     hi            ATRcas
041000070327     c                   eval      v1dcas=dectel
041100070327    2c                   endif
041200070327    1c                   endif
041300070327     c
041400070327     c* Blocco Spermercati/Appuntamento
041500070327     c* Escludo se non da visualizz0re
041600070327     c     'S'           lookup    BLOV                                   34
041700070327     c     'A'           lookup    BLOV                                   35
041800070327    1c                   if        not *in34 and (ARBTC1='S' or arbtc2='S')
041900070402     c                   eval      v1cbas='Supermercati ....:'
042000070327     c                   seton                                        23
042100070327     c* non abilitato
042200070327    2c                   if        §B4Lbas<>' '
042300070327     c                   movel     §b4lbas       wtel              1
042400070327     c                   exsr      DECODITEL
042500070327     c                   eval      v1dbas=dectel
042600070327     c                   movel     hi            ATRbas
042700070327     c                   else
042800070327     c                   eval      v1dbas='ABILITATO'
042900070327    2c                   endif
043000070327    1c                   endif
043100070327     C
043200070327    1c                   if        not *in35 and (ARBTC1='A' or arbtc2='A')
043300070402     c                   eval      v1cbas='Appuntamento ....:'
043400070327     c                   seton                                        23
043500070327     c* non abilitato
043600070327    2c                   if        §B4Lbas<>' '
043700070327     c                   movel     §b4lbas       wtel              1
043800070327     c                   exsr      DECODITEL
043900070327     c                   eval      v1dbas=dectel
044000070327     c                   movel     hi            ATRbas
044100070327     C                   ELSE
044200070327     c                   eval      v1dbas='ABILITATO'
044300070327    2c                   endif
044400070327    1c                   endif
044500070327     c* Fermo Deposito
044600070327     c* Escludo se non da visualizzare
044700070327     c     'F'           lookup    BLOV                                   35
044800070327    1c                   if        not *in35  and arbffd='S'
044900070327     c                   seton                                        24
045000070327    2c                   if        §B4LTFD<>' '
045100070327     c                   movel     §b4ltfd       wtel              1
045200070327     c                   exsr      DECODITEL
045300070327     c                   eval      v1dtfd=dectel
045400070327     c                   movel     hi            ATRtfd
045500070327     c                   else
045600070327     c                   eval      v1dtfd='ABILITATO'
045700070327    2c                   endif
045800070327    1c                   endif
045900070327     c* Blocco consegna
046000070327     c* Escludo se non da visualizzare
046100070327     c     'L'           lookup    BLOV                                   35
046200070427    1c                   if        not *in35
046300070327    2c                   if        arbfbc='A'
046400070427     c                   seton                                        26
046500070427     c* Verifico se da telefonare
046600070427     c     karb          setgt     fnevb22l
046700070427     c     karb          readpe    fnevb22l
046800070427     c
046900070427     c                   if        not %eof(fnevb22l) and (evb2tel='S'
047000070427     c                             or  evb2tel='T' or evb2tel='N')
047100070427     c                   movel     evb2tel       wtel              1
047200070427     c                   exsr      DECODITEL
047300070427     c                   eval      v1dlav=dectel
047400070427     c                   movel     hi            ATRlav
047500070427     c                   else
047600070427     c                   eval      v1dlav='Non Previsto Contatto'
047700070327     C                   endif
047800070427     C                   endif
047900070427     c
048000070327    2c                   if        arbfbc='G'
048100070427     c                   seton                                        25
048200070327     c                   eval      v1dfbc='GIACENZA'
048300070327     C                   endif
048400070327    2c                   if        arbfbc='B'
048500070427     c                   seton                                        25
048600070329     c                   eval      v1dfbc='RIMPALLO'
048700070327    2C                   endif
048800070327    1C                   endif
048900070327     c
049000070327     c                   ENDSR
049100070327     c
049200070327     C**************************************************************************
049300070327     c     DECODITEL     BEGSR
049400070403     c                   clear                   dectel           32
049500070327     c                   select
049600070327     c                   WHEN      wtel='S'
049700070327     c                   eval      dectel='Da Contattare'
049800070327     c
049900070327     c                   when      wtel='T'
050000070327     c                   eval      dectel='Da Ricontattare'
050100070327     c
050200070327     c                   when      wtel='N'
050300070403     c                   eval      dectel='Esclusa Gestione:attesaGiacenza'
050400070327     c
050500070327     c                   other
050600070327     c                   eval      dectel='OK DA TELEF.MA NO ABIL???'
050700070327     C                   endsl
050800070327     c
050900070326     c                   endsr
051000070403     C**************************************************************************
051100070403     c     STORICOnote   BEGSR
051200070403     c* carico sotirco note in SFL
051300070403     c                   EXSR      CARSFL
051400070403     c                   z-add     1             recsf1
051500070416     c                   if        nrr=0
051600070416     c                   seton                                        35
051700070416     c                   endif
051800070403     c
051900131120     c                   write     LRG7z04
052000131120     c                   exfmt     LRG7c04
052100070403     c
052200070403     c                   endsr
052300070403     C**************************************************************************
052400070403     c     CARsfl        BEGSR
052500070403     c* Pulizia SFL
052600070416     C                   SETON                                        3536
052700131120     C                   WRITE     LRG7c04
052800070416     C                   SETOFF                                       3536
052900070403     c                   clear                   nrr               4 0
053000070403     c
053100070403     c* Carico tutte le note della sospensione consegna
053200070403     c     karn          setll     fiarn12l
053300070403     c     karn          reade     fiarn12l
053400070403    0c                   dow       not %eof(fiarn12l)
053500070403     c
053600070403     c* emetto in RI  utente data e ora
053700070403     c                   seton                                        11
053800070403     c                   clear                   w4snot1
053900070403     c     *iso          movel     arndim        dataeur
054000070403     c                   movel     dataeur       w0080             8 0
054100070403     c                   eval      w4snot1='Inserita da ' +ARNPRU+ 'il '+
054200070403     c                             %editw(w0080: '  /  /    ')+ ' ore ' +
054300070403     c                             %editw(arnoim: '  :  :  ')
054400070403     c                   clear                   w4sbge
054500070403     c                   add       1             nrr
054600131120     c                   write     LRG7s04
054700070403     c                   setoff                                       11
054800070403     c* Scrivo le note
054900070403    1c                   if        arnfl1='S'
055000070403     c                   eval      w4sbge='  SI '
055100070403     c                   else
055200070403     c                   eval      w4sbge='TOLTA'
055300070403    1c                   endif
055400070403     c
055500070403     C                   MOVEL     ARNNOB        W4SNOT1
055600070403     c                   add       1             nrr
055700131120     c                   write     LRG7s04
055800070403     c
055900070403     C* SE SONO PIENI GLI ULTIMI 11 CARATTERI, scrivo anche questi
056000070403     c                   if        %subst(arnnob:61:10)<>*blanks
056100070403     c                   clear                   w4sbge
056200070403     c                   clear                   w4snot1
056300070403     c                   eval      w4snot1=%subst(arnnob:61:10)
056400070403     c                   add       1             nrr
056500131120     c                   write     LRG7s04
056600070403    1c                   endif
056700070403     c
056800070403     c     karn          reade     fiarn12l
056900070403    0c                   enddo
057000070403     c                   endsr
057100070313**
057200070313Numero di spedizione inesistente
057300070327**
057400070330 ** VISUALIZZAZIONE ANALITICA ABILITAZIONI **
