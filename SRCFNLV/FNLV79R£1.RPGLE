000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200110523     H* FNLV79R *----------------------------------------------------*
000300110523     H* - Interrogazione Email Destinatario
000400000000     H*--------------------------------------------------------------*
000500070326     FFiar401L  IF   E           K DISK
000600070316     FFNARB01L  IF   E           K DISK
000700070427     FFNEVB22L  IF   E           K DISK
000800070327     Ffiarn12l  iF a E           K DISK
000900070327     Ftabel00f  IF   E           K DISK
001000070419     FFNLRg6D   CF   E             WORKSTN usropn
001100070403     F                                     SFILE(LRg6s04:NRR)
001200030722     F**
001300070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001400070330     D TES             S             45    DIM(1) CTDATA PERRCD(1)
001500070426     D ABI             S              1    DIM(15)
001600070313     d
001700051123      * ds di passaggio dati
001800110523     D fnlv79ds      E DS
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
003500070327     d Dataeur         s               d   datfmt(*eur)
003600070327     D* CAMPO PER DSPATR "UL"
003700070327     D hi              C                   CONST(X'22')
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000110523     C                   PARM                    fnlv79ds
004100070327     C                   PARM                    dspararb
004200070327     C                   PARM                    dsparar4L
004300940223     C*---------------------------------------------------------------*
004400070326     C* TIPO LANCIO, ilrG6TLA "C" -           CHIUSO CON LR
004500070326     C* TIPO LANCIO, ilrG6TLA "L" - ELABORO E CHIUDO CON LR
004600070326     C* TIPO LANCIO, ilrG6TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800070326    1C     ilrG6TLA      IFNE      'C'
004900070326     c                   clear                   olrG6err
005000070326     c                   clear                   olrG6msg
005100070326     c                   clear                   olrG6vssin
005200070426     c                   clear                   olrG6ABI
005300070426     c                   clear                   ABI
005400070412     c
005500051123     c
005600070313     C* SE NON PASSATo record spedizione, lo chaino
005700070327     C                   IF        (%parms>=2 and parnsp=0) or
005800070315     c                              %parms=1
005900070313     c
006000070313     c     karb          chain     fnarb01l
006100070313     c                   if        not %found(fnarb01l)
006200070313     c* restituisco errore al chiamante ed esco
006300070326     c                   eval      olrG6msg=msg(1)
006400070326     c                   eval      olrG6err='E'
006500070313     c                   goto      fine
006600070313     c                   endif
006700070327     c                   else
006800070327     c                   movel     dspararb      dsrecarb
006900070313     c                   endif
007000070326     c
007100070327     C                   IF        (%parms=3 and pa4nsp=0) or
007200070326     c                              %parms<3
007300070326     c
007400070326     c     kar4          chain     fiar401l
007500070326     c                   if        not %found(fiar401l)
007600070402     c* Se non trovato clearo il campo ar4not
007700070402     c                   clear                   ar4not
007800070402     c                   clear                   ar4aas
007900070402     c                   clear                   ar4lnp
008000070402     c                   clear                   ar4nrs
008100070402     c                   clear                   ar4nsp
008200070402     c                   clear                   ar4trc
008300070326     c                   endif
008400070327     c                   else
008500070327     c                   movel     dsparar4L     dsrecar4L
008600070326     c                   endif
008700070326     c* riempi la DSL4L
008800070326     c                   movel     ar4not        dsbl4l
008900070326     c
009000070326     c* se passato '?' visulizzo finestra con situazione ANALITICa
009100070326     C                   IF        ILRG6VSDET='?'
009200070419     c
009300070419     c                   open      fnlrg6d
009400070419     c
009500070412     c                   write     lrg6d02
009600070326     c                   exsr      VISUALDETT
009700070327     c* Emetto videata
009800070327     c
009900070403     c                   setoff                                       kl
010000070403     c                   dow       not *inkl
010100070327     c                   exfmt     lrg6w01
010200070403     c
010300070403     c* Se messo '?' su interrogazione storico sospensione, emetto altra vid
010400070403     c                   if        v1csto='?'
010500070403     c                   exsr      STORICOnote
010600070403     c                   clear                   v1csto
010700070403     c                   setoff                                       kl
010800070403     c                   endif
010900070403     c
011000070403     c                   enddo
011100070403     c
011200070327     c
011300070326     c                   else
011400070326     c*  altrimenti restituisco riga SINTETICA dei dati
011500070326     c                   exsr      VISUALSIN
011600070326     c                   endif
011700070314     c
011800070314     c     fine          TAG
011900070314     c
012000070314     c                   endif
012100051123     c
012200070326     C     IlrG6tla      IFNE      ' '
012300070412     C     ILRG6VSDET    oreq      '?'
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700051123     C                   ENDIF
012800991206     C**************************************************************************
012900051123     C     *INZSR        BEGSR
013000051123     c
013100070313     c     kARB          Klist
013200070326     c                   Kfld                    ilrG6Aas
013300070326     c                   Kfld                    ilrG6Lnp
013400070326     c                   Kfld                    ilrG6nrs
013500070326     c                   Kfld                    ilrG6nsp
013600070326     c     kAR4          Klist
013700070326     c                   Kfld                    ilrG6Aas
013800070326     c                   Kfld                    ilrG6Lnp
013900070326     c                   Kfld                    ilrG6nrs
014000070326     c                   Kfld                    ilrG6nsp
014100070326     c                   Kfld                    KTRC
014200051123     c
014300070327     C     KTAB1         KLIST
014400070327     C                   KFLD                    CODUT
014500070327     C                   KFLD                    COD
014600070327     C                   KFLD                    KEY
014700070327     C     KARN          KLIST
014800070327     C                   KFLD                    arbAAS
014900070327     C                   KFLD                    arbLNP
015000070327     C                   KFLD                    arbNRS
015100070327     C                   KFLD                    arbNSP
015200070327     C                   KFLD                    kcdn
015300070313     c                   endsr
015400070313     C**************************************************************************
015500070326     C     VISUALSIN     BEGSR
015600070327     c                   clear                   wrigaSIN        100
015700070327     c                   clear                   wcorta            1
015800070327     c                   exsr      DECODI
015900070327     c*
016000070327     c* Verifico se la decodifica lunga c'e' stata entro 50caratteri
016100070327     c     ' '           checkr    wrigaSIN      posiz             3 0    34
016200070327     c
016300070327     c                   select
016400070327     c                   when      (*in34 and posiz<=50 and
016500070327     c                             %subst(WrigaSIN:1:1)<>'-')
016600070327     c                   eval      olrg6VSSIN=wrigasin
016700070327     c
016800070327     c                   when      (*in34 and posiz<=51 and
016900070327     c                             %subst(WrigaSIN:1:1)='-')
017000070327     c                   eval      olrg6VSSIN=%subst(wrigaSIN:2:50)
017100070327     c                   other
017200070327     c
017300070327     c* Versione più ridotta perchè non ci sta
017400070327     c                   clear                   wrigaSIN
017500070427     c                   clear                   ABI
017600070327     c                   eval      wcorta='S'
017700070327     c                   exsr      DECODI
017800070327     c
017900070327     c                   if        %subst(wrigaSIN:1:1)='-'
018000070327     c                   eval      olrg6VSSIN=%subst(wrigaSIN:2:50)
018100070327     c                   else
018200070327     c                   eval      olrg6VSSIN=wrigasin
018300070327     c                   endif
018400070327     c                   ENDSL
018500070326     c
018600070427     c                   movea     ABI           olrg6abi
018700070326     c*
018800070326     c                   endsr
018900070327     C**************************************************************************
019000070327     c     DECODI        BEGSR
019100070327     c* Blocco Generico
019200070327     c* Escludo se non da visualizzare
019300070327     c     'O'           lookup    BLOV                                   35
019400070327    1c                   if        not *in35 and §B4LBGE='S'
019500070426     c*
019600070426     c                   movel     §b4lbge       abi(1)
019700070327    2c                   if        wcorta=' '
019800070327     c                   eval      wrigaSIN='SospensCONS'
019900070327     c                   else
020000070327     c                   eval      wrigaSIN='SospCONS'
020100070327    2c                   endif
020200070327    1c                   endif
020300070327     c* Blocco ASSEGNATO
020400070327     c* Escludo se non da visualizzare
020500070327     c     '9'           lookup    BLOV                                   35
020600070327    1c                   if        not *in35 and ARBACA=' '
020700070426     c                   movel     'S'           abi(2)
020800070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
020900070327     c                             '-ASSEGNATO'
021000070327    1c                   endif
021100070327     c* Blocco C/Assegno
021200070327     c* Escludo se non da visualizzare
021300070327     c     'C'           lookup    BLOV                                   35
021400070430    1c*****              if        not *in35 and (ARBACC=' ' or §B4LTCA<>' ')
021500070430    1c                   if        not *in35 and (ARBACC=' ')
021600070426     c                   movel     §b4ltca       abi(3)
021700070327    2c                   if        wcorta=' '
021800070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
021900070327     c                             '-C/ASSEGNO'
022000070327     c                   else
022100070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
022200070327     c                             '-C/ASS'
022300070327    2c                   endif
022400070327    1c                   endif
022500070426     c* Blocco Supermercati/Appuntamento
022600070327     c* Escludo se non da visualizz0re
022700070327     c     'S'           lookup    BLOV                                   34
022800070327     c     'A'           lookup    BLOV                                   35
022900070327    1c                   if        §B4Lbas<>' '
023000070327    2c                   if        not *in34 and (ARBTC1='S' or arbtc2='S')
023100070426     c                   movel     §b4lbas       abi(4)
023200070327    3c                   if        wcorta=' '
023300070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
023400070403     c                             '-SUPERMERCATO'
023500070327     c                   else
023600070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
023700070327     c                             '-SUPERM'
023800070327    3c                   endif
023900070327    2c                   endif
024000070327    2c                   if        not *in35 and (ARBTC1='A' or arbtc2='A')
024100070504     c                   movel     §b4lbas       abi(5)
024200070327    3c                   if        wcorta=' '
024300070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
024400070403     c                             '-APPUNTAMENTO'
024500070327     c                   else
024600070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
024700070327     c                             '-APPUNT'
024800070327    3c                   endif
024900070327    2c                   endif
025000070327    1c                   endif
025100070327     c* Fermo Deposito
025200070327     c* Escludo se non da visualizzare
025300070327     c     'F'           lookup    BLOV                                   35
025400070327    1c                   if        not *in35 and §B4LTFD<>' ' and arbffd='S'
025500070504     c                   movel     §b4ltfd       abi(6)
025600070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
025700070327     c                             '-FDEPOSITO'
025800070327    1c                   endif
025900070327     c* Blocco consegna
026000070327     c* Escludo se non da visualizzare
026100070327     c     'L'           lookup    BLOV                                   35
026200070427     c                   if        not *in35
026300070427     c
026400070427     c* Lasciato avviso: solo se da telefonare:lo  vedo in EVB2
026500070427    1c                   if        arbfbc='A'
026600070427     c     karb          setgt     fnevb22l
026700070427     c     karb          readpe    fnevb22l
026800070427     c
026900070427     c                   if        not %eof(fnevb22l) and (evb2tel='S'
027000070427     c                             or  evb2tel='T' or evb2tel='N')
027100070427     c
027200070504     c                   movel     evb2tel       abi(7)
027300070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
027400070403     c                             '-LascAVVISO'
027500070327    2c                   endif
027600070427    2c                   endif
027700070427     c
027800070327    2c                   if        arbfbc='G'
027900070504     c                   movel     arbfbc        abi(8)
028000070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
028100070327     c                             '-GIACENZA'
028200070327    2c                   endif
028300070327    2c                   if        arbfbc='B'
028400070504     c                   movel     arbfbc        abi(8)
028500070327     c                   eval      wrigaSIN=%trim(wrigaSIN)+
028600070329     c                             '-RIMPALLO'
028700070327    2c                   endif
028800070427    1c                   endif
028900070327     c                   ENDSR
029000070326     C**************************************************************************
029100070326     C     VISUALDETT    BEGSR
029200070327     c                   clear                   lrg6w01
029300070327     c                   movea     blov          w010a            10
029400070330     C                   MOVE      tes(1)        V1CTES
029500070327     c
029600070327     c                   eval      v1clnp=arblnp
029700070327     c                   eval      v1cnrs=arbnrs
029800070327     c                   eval      v1cnsp=arbnsp
029900070403     c                   clear                   v1csto
030000070327     c* Data Spedizione
030100070327     c                   movel     arbaas        w0080
030200070327     c                   move      arbmgs        w0080             8 0
030300070327     c     *iso          movel     w0080         dataeur
030400070327     c                   movel     dataeur       v1cdsp
030500070327     c* codice bolla
030600070327     C                   MOVEL(P)  ARBCBO        KEY
030700070327     C     KTAB1         CHAIN     TABEL                              31
030800070327     C  n31              MOVEL     TBLUNI        DS3A
030900070327     C   31              clear                   DS3A
031000070327     c                   movel     §3atb1        flgtb1            1
031100070327     c
031200070327     c* Blocco Generico
031300070327     c* Escludo se non da visualizzare
031400070327     c     'O'           lookup    BLOV                                   35
031500070403    1c                   if        not *in35
031600070403     c
031700070403    2c                   if        §B4LBGE='S'
031800070327     c                   eval      v1dbge='SI'
031900070327     c                   seton                                        20
032000070403     c                   movel     hi            ATRbge
032100070403    2c                   endif
032200070403     c
032300070327     c* chain fiarn ultimo record motivo sospensione generica
032400070327     c     karn          chain     fiarn12l
032500070327    2c                   if        %found(fiarn12l)
032600070327     c                   movel     arnnob        v1cnob1
032700070327     c                   move      arnnob        v1cnob2
032800070403    3c                   if        not *in20
032900070403     c                   seton                                        20
033000070403     c                   eval      v1dbge='NO'
033100070403    3c                   endif
033200070403    2c                   endif
033300070327    1c                   endif
033400070327     c* Blocco ASSEGNATO
033500070327     c* Escludo se non da visualizzare
033600070327     c     '9'           lookup    BLOV                                   35
033700070327    1c                   if        not *in35
033800070327    2c                   if        ARBACA=' '
033900070327     c                   seton                                        21
034000070327     c                   eval      v1dass='Da Abilitare'
034100070327     c                   movel     hi            ATRASS
034200070327     c                   else
034300070327     c* Se bolla in assegnato dico se C/A già abilitato
034400070327    3c                   if        flgtb1='A' or §3atb2<>*blanks
034500070327     c                   seton                                        21
034600070327     c                   eval      v1dass='ABILITATO   '
034700070327    3C                   ENDIF
034800070327    2C                   ENDIF
034900070327    1C                   ENDIF
035000070327     c* Blocco C/Assegno
035100070327     c* Escludo se non da visualizzare
035200070327     c     'C'           lookup    BLOV                                   35
035300070327    1c                   if        not *in35 and §3afca<>0
035400070327     c                   seton                                        22
035500070327    2c                   if        ARBACC='S'
035600070327     c                   eval      v1dCAS='ABILITATO    '
035700070327     c                   else
035800070327     c                   movel     §b4ltca       wtel              1
035900070327     c                   exsr      DECODITEL
036000070327     c                   movel     hi            ATRcas
036100070327     c                   eval      v1dcas=dectel
036200070327    2c                   endif
036300070327    1c                   endif
036400070327     c
036500070327     c* Blocco Spermercati/Appuntamento
036600070327     c* Escludo se non da visualizz0re
036700070327     c     'S'           lookup    BLOV                                   34
036800070327     c     'A'           lookup    BLOV                                   35
036900070327    1c                   if        not *in34 and (ARBTC1='S' or arbtc2='S')
037000070402     c                   eval      v1cbas='Supermercati ....:'
037100070327     c                   seton                                        23
037200070327     c* non abilitato
037300070327    2c                   if        §B4Lbas<>' '
037400070327     c                   movel     §b4lbas       wtel              1
037500070327     c                   exsr      DECODITEL
037600070327     c                   eval      v1dbas=dectel
037700070327     c                   movel     hi            ATRbas
037800070327     c                   else
037900070327     c                   eval      v1dbas='ABILITATO'
038000070327    2c                   endif
038100070327    1c                   endif
038200070327     C
038300070327    1c                   if        not *in35 and (ARBTC1='A' or arbtc2='A')
038400070402     c                   eval      v1cbas='Appuntamento ....:'
038500070327     c                   seton                                        23
038600070327     c* non abilitato
038700070327    2c                   if        §B4Lbas<>' '
038800070327     c                   movel     §b4lbas       wtel              1
038900070327     c                   exsr      DECODITEL
039000070327     c                   eval      v1dbas=dectel
039100070327     c                   movel     hi            ATRbas
039200070327     C                   ELSE
039300070327     c                   eval      v1dbas='ABILITATO'
039400070327    2c                   endif
039500070327    1c                   endif
039600070327     c* Fermo Deposito
039700070327     c* Escludo se non da visualizzare
039800070327     c     'F'           lookup    BLOV                                   35
039900070327    1c                   if        not *in35  and arbffd='S'
040000070327     c                   seton                                        24
040100070327    2c                   if        §B4LTFD<>' '
040200070327     c                   movel     §b4ltfd       wtel              1
040300070327     c                   exsr      DECODITEL
040400070327     c                   eval      v1dtfd=dectel
040500070327     c                   movel     hi            ATRtfd
040600070327     c                   else
040700070327     c                   eval      v1dtfd='ABILITATO'
040800070327    2c                   endif
040900070327    1c                   endif
041000070327     c* Blocco consegna
041100070327     c* Escludo se non da visualizzare
041200070327     c     'L'           lookup    BLOV                                   35
041300070427    1c                   if        not *in35
041400070327    2c                   if        arbfbc='A'
041500070427     c                   seton                                        26
041600070427     c* Verifico se da telefonare
041700070427     c     karb          setgt     fnevb22l
041800070427     c     karb          readpe    fnevb22l
041900070427     c
042000070427     c                   if        not %eof(fnevb22l) and (evb2tel='S'
042100070427     c                             or  evb2tel='T' or evb2tel='N')
042200070427     c                   movel     evb2tel       wtel              1
042300070427     c                   exsr      DECODITEL
042400070427     c                   eval      v1dlav=dectel
042500070427     c                   movel     hi            ATRlav
042600070427     c                   else
042700070427     c                   eval      v1dlav='Non Previsto Contatto'
042800070327     C                   endif
042900070427     C                   endif
043000070427     c
043100070327    2c                   if        arbfbc='G'
043200070427     c                   seton                                        25
043300070327     c                   eval      v1dfbc='GIACENZA'
043400070327     C                   endif
043500070327    2c                   if        arbfbc='B'
043600070427     c                   seton                                        25
043700070329     c                   eval      v1dfbc='RIMPALLO'
043800070327    2C                   endif
043900070327    1C                   endif
044000070327     c
044100070327     c                   ENDSR
044200070327     c
044300070327     C**************************************************************************
044400070327     c     DECODITEL     BEGSR
044500070403     c                   clear                   dectel           32
044600070327     c                   select
044700070327     c                   WHEN      wtel='S'
044800070327     c                   eval      dectel='Da Contattare'
044900070327     c
045000070327     c                   when      wtel='T'
045100070327     c                   eval      dectel='Da Ricontattare'
045200070327     c
045300070327     c                   when      wtel='N'
045400070403     c                   eval      dectel='Esclusa Gestione:attesaGiacenza'
045500070327     c
045600070327     c                   other
045700070327     c                   eval      dectel='OK DA TELEF.MA NO ABIL???'
045800070327     C                   endsl
045900070327     c
046000070326     c                   endsr
046100070403     C**************************************************************************
046200070403     c     STORICOnote   BEGSR
046300070403     c* carico sotirco note in SFL
046400070403     c                   EXSR      CARSFL
046500070403     c                   z-add     1             recsf1
046600070416     c                   if        nrr=0
046700070416     c                   seton                                        35
046800070416     c                   endif
046900070403     c
047000070403     c                   write     lrg6z04
047100070403     c                   exfmt     lrg6c04
047200070403     c
047300070403     c                   endsr
047400070403     C**************************************************************************
047500070403     c     CARsfl        BEGSR
047600070403     c* Pulizia SFL
047700070416     C                   SETON                                        3536
047800070403     C                   WRITE     LRg6c04
047900070416     C                   SETOFF                                       3536
048000070403     c                   clear                   nrr               4 0
048100070403     c
048200070403     c* Carico tutte le note della sospensione consegna
048300070403     c     karn          setll     fiarn12l
048400070403     c     karn          reade     fiarn12l
048500070403    0c                   dow       not %eof(fiarn12l)
048600070403     c
048700070403     c* emetto in RI  utente data e ora
048800070403     c                   seton                                        11
048900070403     c                   clear                   w4snot1
049000070403     c     *iso          movel     arndim        dataeur
049100070403     c                   movel     dataeur       w0080             8 0
049200070403     c                   eval      w4snot1='Inserita da ' +ARNPRU+ 'il '+
049300070403     c                             %editw(w0080: '  /  /    ')+ ' ore ' +
049400070403     c                             %editw(arnoim: '  :  :  ')
049500070403     c                   clear                   w4sbge
049600070403     c                   add       1             nrr
049700070403     c                   write     lrg6s04
049800070403     c                   setoff                                       11
049900070403     c* Scrivo le note
050000070403    1c                   if        arnfl1='S'
050100070403     c                   eval      w4sbge='  SI '
050200070403     c                   else
050300070403     c                   eval      w4sbge='TOLTA'
050400070403    1c                   endif
050500070403     c
050600070403     C                   MOVEL     ARNNOB        W4SNOT1
050700070403     c                   add       1             nrr
050800070403     c                   write     lrg6s04
050900070403     c
051000070403     C* SE SONO PIENI GLI ULTIMI 11 CARATTERI, scrivo anche questi
051100070403     c                   if        %subst(arnnob:61:10)<>*blanks
051200070403     c                   clear                   w4sbge
051300070403     c                   clear                   w4snot1
051400070403     c                   eval      w4snot1=%subst(arnnob:61:10)
051500070403     c                   add       1             nrr
051600070403     c                   write     lrg6s04
051700070403    1c                   endif
051800070403     c
051900070403     c     karn          reade     fiarn12l
052000070403    0c                   enddo
052100070403     c                   endsr
052200070313**
052300070313Numero di spedizione inesistente
052400070327**
052500070330 ** VISUALIZZAZIONE ANALITICA ABILITAZIONI **
