000100041104     H DECEDIT('0,') DATEDIT(*ymd.)
000200950130     F* FNLV52R *-----------------------------------------------------*
000300950130     F*       AGGIORNAMENTI VARI SULLE BOLLE
000400950130     F*---------------------------------------------------------------*
000500091021     FFNblp46l  IF   E           K DISK    rename(fnblp000:fnablp046)
000600091021     FFNBTT11L  IF   E           K DISK
000700031127     FFNBTP01L  UF   E           K DISK
000800950130     F*
000900950130     FFNART01L  IF   E           K DISK
001000950130     FFNARB01L  UF   E           K DISK
001100950130     F*
001200950130     FFNBLT01L  IF   E           K DISK
001300111006     FFNBLP01L  UF   E           K DISK
001400950130     F*
001500070507     FFNAGB02L  IF   E           K DISK    infds(fnagbinf)
001600070507     FFNAGB00F  UF   E             DISK    rename(fnagb000:fnagbf00)
001700041108     FFNAGP00f  UF   E             DISK
001800041108     FFNAGv00f  UF   E             DISK
001900030108     fFidta01l  uf a e           K Disk
002000030320     fTabel00f  if   e           k Disk
002100111007      * Bolle sede
002200111007     fTitas30c  uf   e           k Disk    UsrOpn extfile(tas30)
002300150605     FFNBRV07L  IF   E           K DISK
002400150605      * clienti/filiali con sospensione bolle
002500150605     ffibsp01l  if   e           k disk
002600150605     ffibsp02l  if   e           k disk    rename(fibsp000:fibsp02)
002700150605
002800150605     fazorg01l  if   e           k disk
002900030108
003000041129     D WNFV            S              5  0 DIM(2)
003100160422
003200160422     D digits          c                   '0123456789'
003300030108
003400041108     d oldpkc          s                   Like(blppkc)
003500041108     d oldncp          s                   Like(blpncp)
003600041108     d oldpkf          s                   Like(blppkf)
003700041108     d oldvlc          s                   Like(blpvlc)
003800041108     d oldncr          s                   Like(blpncr)
003900041108     d oldvlf          s                   Like(blpvlf)
004000070705     d oldFVF          s                   Like(blpFVF)
004100041108     d SavPoc          s                   Like(DtaPoc)
004200030108     d SavTrd          s                   Like(DtaTrd)
004300030109     d wFidta          s              1    Inz('0')
004400110701     d $err            s              1    Inz('0')
004500030320     d Cod             s                   Like(TblCod)
004600030320     d Key             s                   Like(TblKey)
004700040401     d wdtadam         s                   Like(Arbdam)
004800040401     d wdtadet         s                   Like(btpdet)
004900040401     d wdtadut         s                   Like(btpdut)
005000041126     d wdtaduc         s                   Like(blpduc)
005100130204     d wagg1           s              1
005200130206     d parvar          s            224
005300150605     d w_clibo         s                   Like(blpccm)
005400150605     d wncd            s                   Like(blpncd)
005500150605     d wnca            s                   Like(blpnca)
005600150605     d wlnpb           s                   Like(brvfle)
005700150605     d wdcs            s                   Like(brvdcs)
005800150605     d whcs            s                   Like(brvhcs)
005900150605     d wdfv            s                   Like(d53dfv)
006000150605     d wBspTp          s              1
006100150625     d idx             s              3  0
006200030108
006300070507     D fnagbinf        ds
006400070507     D  fnagbnrr             397    400i 0
006500960229     D                 DS
006600960229     D  ARBAAS                 1      4  0
006700960229     D  ARBMGS                 5      8  0
006800960229     D  ARBDSP                 1      8  0
006900970414     D WLBDAT          DS                  INZ
007000970414     D  G02DAT                 1      8  0
007100970414     D  G02INV                 9     16  0
007200970414     D  G02ERR                17     17
007300970414     D  G02TGI                18     22  0
007400970414     D WGIDAT          DS                  INZ
007500970414     D  GIODAT                 1      8  0
007600970414     D  GIOINV                 9     16  0
007700970414     D  GIOTGI                17     21  0
007800150605     d                 ds
007900150605     d bsplin
008000150605     d sbsp                           3  0 dim(30) overlay(bsplin)
008100111006     d
008200950130     D* PARAMETRI DI PASSAGGIO
008300950130     D PARAM           DS
008400950130     D  PARTBO                 1      1
008500111006     d
008600950130     D KPJBA         E DS
008700150605     D* REPERISCE DATI FOGLI
008800150605     D DSLV53        E DS                  EXTNAME(FNLV53DS)
008900031016      * DS Esterna x gestione allocazione ed invio messaggi
009000031016     D TRUL82DS      E DS
009100041108     D fnlv39ds      E DS
009200041108     D fnlv20ds      E DS
009300041108     D fnlv57ds      E DS
009400130204     D fnlvv1ds      E DS
009500150605     D fnls65ds      E DS
009600150625     D trul06ds      E DS
009700150625     D  L06                    1     90  0 dim(30)
009800030320
009900030320     d ds3a          e ds
010000150605     d og143         e ds
010100110701     d
010200111007     d TAS30           s             21    inz('GAITRAGRPS/TITAS30C')
010300130124     D                SDS
010400130124     D  NOMPGM                 1     10
010500111007     d
010600111007     ITITAS000      02
010700111007     ITITAS010      03
010800111007     ITITASP00      04
010900950130     I*----------------------------------------------------------------
011000950130     C     *ENTRY        PLIST
011100950130     C                   PARM                    KPJBA
011200040514     C                   PARM                    chiudi            1
011300111006     c
011400040514     C                   SETOFF                                       30
011500111005     c                   clear                   ApriLV57          1
011600111007     c
011700111007     c* Se non aperto apro il file
011800111007     c                   if        not %open(titas30c)
011900111007     C                   OPEN(e)   titas30c
012000111007     c
012100111007     c                   if        not %open(titas30c)
012200111007     c                   eval      %subst(tas30:7:4)='GRU '
012300111007     C                   OPEN      titas30c
012400111007     c                   endif
012500111007     c                   endif
012600950130     C*
012700950130     C* LETTURA DEL FILE
012800970117    1C     PARTBO        IFEQ      ' '
012900070504     C     *LOVAL        SETLL     FNAGB02L
013000950130     C                   ELSE
013100070504     C     PARTBO        SETLL     FNAGB02L
013200970117    1C                   ENDIF
013300040315     c
013400040315    1c                   dow       not *in30
013500950130     C*
013600040315    2C     PARTBO        IFEQ      ' '
013700070507     C                   READ      FNAGB02L                               30
013800950130     C                   ELSE
013900070507     C     PARTBO        READE     FNAGB02L                               30
014000040315    2C                   ENDIF
014100950130     C*
014200111006     c* Escludo tipo bolla "S"
014300111010    2C                   if        not *in30
014400950130     C*
014500950130     C                   EXSR      ELAB
014600040309     c
014700070507    3c                   if        werror='E'
014800040309     c                   goto      fine
014900070507    3c                   endif
015000070507    2C                   ENDIF
015100950130     C*
015200040315    1C                   ENDDO
015300040315     c
015400041108     C
015500041108     c     fine          tag
015600040309     c
015700041104     C* ELABORA LE BOLLE PER IL PESO
015800041104     C                   EXSR      ELAAGP
015900041108     C* ELABORA LE BOLLE PER IL volume
016000041108     C                   EXSR      ELAAGV
016100111005
016200111005     c                   if        ApriLV57='S'
016300111005     c                   clear                   fnlv57ds
016400111005     c                   eval      i57tla='C'
016500111005     c                   call      'FNLV57R'
016600111005     C                   PARM                    KPJBA
016700111005     C                   PARM                    fnlv57ds
016800111005     c                   endif
016900130201     C                   MOVEL     'C'           iv1tla
017000130201     c                   movel     fnlvv1ds      kpjbu
017100130206     C                   CALL      'FNLVV1R'
017200130201     C                   PARM                    kpjba
017300150605
017400150605     C                   CLEAR                   fnls65ds
017500150605     C                   MOVEL     'C'           i65tla
017600150605     c                   movel     '1'           I65STRCMT
017700150605     c                   movel     fnls65ds      kpjbu
017800150605     C                   CALL      'FNLS65R'
017900150605     C                   PARM                    kpjba
018000150625
018100150625     c                   clear                   trul06ds
018200150625     c                   movel     'C'           d06tla
018300150625     c                   movel     trul06ds      kpjbu
018400150625     C                   CALL      'TRUL06R'
018500150625     C                   PARM                    kpjba
018600130201     c
018700040514     C*
018800040514     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
018900040514     C                   SHTDN                                        98
019000040514     c
019100040514     c                   if        *in98
019200040514     c                   eval      chiudi='S'
019300111006     C****               SETON                                        LR
019400040514     c                   else
019500111005
019600111006     C****               SETON                                        RT
019700040514     c                   endif
019800111006     c
019900111006     C                   SETON                                        LR
020000950130     C*
020100950130     C**************************************************************************
020200950130     C* AGGIORNO BOLLE  ARRIVI
020300950130     C**************************************************************************
020400950130     C     AGGARB        BEGSR
020500950130     C                   MOVEL     *HIVAL        WDPC
020600950130     C                   MOVEL     *HIVAL        WDET
020700950130     C                   MOVEL     *HIVAL        WDAM
020800950130     C                   Z-ADD     *ZERO         WDUT
020900950130     C                   Z-ADD     *ZERO         WDUC
021000950328     C                   Z-ADD     *ZERO         WDAU
021100950122     C                   CLEAR                   WFAN
021200951026     C                   SETOFF                                       99
021300000000     C*
021400000000     C     KSAV          SETLL     FNART01L
021500000000     C     KSAV          READE     FNART01L                               34
021600000000     C*
021700950130    1C     *IN34         DOWEQ     '0'
021800950130     C*
021900950130     C* CAUSALE PER AGGIORNAMENTO DATE
022000070504    2C     agbagb        IFEQ      'DT'
022100000000     C*
022200950130     C* DATA ENTRATA AL TRANSITO
022300950130    3C     ARTDET        IFGT      0
022400950130     C     ARTDET        ANDLT     WDET
022500950130     C                   MOVEL     ARTDET        WDET
022600950130    3C                   END
022700000000     C*
022800000000     C* DATA USCITA DAL TRANSITO
022900950130    3C     ARTDUT        IFGT      0
023000950130     C     ARTDUT        ANDGT     WDUT
023100950130     C                   MOVEL     ARTDUT        WDUT
023200950130    3C                   END
023300950130     C*
023400950130     C* DATA PARTENZA 1 COLLO
023500950130    3C     ARTDFV        IFGT      0
023600950130    4C     ARTDFV        IFLT      WDPC
023700950130     C                   MOVEL     ARTDFV        WDPC
023800950130    4C                   END
023900950130     C* DATA PARTENZA ULTIMO COLLO
024000950130    4C     ARTDFV        IFGT      WDUC
024100950130     C                   MOVEL     ARTDFV        WDUC
024200950130    4C                   END
024300950130     C*
024400950130   X3C                   ELSE
024500950130     C                   Z-ADD     *HIVAL        WDUC
024600950130    3C                   END
024700950328     C* CODICE  ANOMALIA
024800950328    3C     ARTCAN        IFNE      *BLANKS
024900950328     C                   MOVEL     'S'           WFAN
025000950328    3C                   ENDIF
025100950328    2C                   END
025200950130     C*
025300950328     C* CAUSALE PER AGGIORNAMENTO DATE
025400070504    2C     agbagb        IFEQ      'DT'
025500950328     C* CAUSALE DI AGGIORNAMENTO DA ABBINAMENTO
025600070504    2C     agbagb        OREQ      'AB'
025700950328     C*
025800950328    3C     ARTDAM        IFGT      0
025900950328    4C     ARTDAM        IFLT      WDAM
026000950328     C                   MOVEL     ARTDAM        WDAM
026100950328    4C                   END
026200950918     C* DATA ARRIVO  ULTIMO COLLO LEGGENDO DA ARTDAM
026300950328    4C     ARTDAM        IFGT      WDAU
026400950328     C                   MOVEL     ARTDAM        WDAU
026500950328    4C                   END
026600950328     C*
026700950328   X3C                   ELSE
026800950328     C                   Z-ADD     *HIVAL        WDAU
026900950328    3C                   END
027000950130    2C                   ENDIF
027100000000     C*
027200000000     C     KSAV          READE     FNART01L                               34
027300950130    1C                   ENDDO
027400000000     C*
027500000000     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
027600000000     C*   TRANSITO E FILIALE TRANSITO
027700951026     C     KSAV          CHAIN     FNARB01L                           3099
027800000000     C*
027900951026    1C     *IN30         IFEQ      *OFF
028000951026     C     *IN99         ANDEQ     *OFF
028100040401     c                   z-add     arbdam        wdtadam
028200950328     C* CAUSALE PER AGGIORNAMENTO DATE
028300070504   1AC     agbagb        IFEQ      'DT'
028400950130     C* DATA ENTRATA AL TRANSITO
028500950130    2C     WDET          IFNE      *HIVAL
028600950130     C                   MOVEL     WDET          ARBDET
028700000000     C                   ELSE
028800000000     C                   MOVEL     *ZEROS        ARBDET
028900950130    2C                   END
029000000000     C* DATA USCITA DAL TRANSITO
029100950130     C                   Z-ADD     WDUT          ARBDUT
029200950130     C* DATA PARTENZA ULTIMO COLLO
029300950130    2C     WDUC          IFNE      *HIVAL
029400950130     C                   MOVEL     WDUC          ARBDUC
029500950130     C                   ELSE
029600950130     C                   MOVEL     *ZEROS        ARBDUC
029700950130    2C                   END
029800950130     C* DATA PARTENZA PRIMO COLLO
029900950130    2C     WDPC          IFNE      *HIVAL
030000950130     C                   MOVEL     WDPC          ARBDPC
030100950130     C                   ELSE
030200950130     C                   MOVEL     *ZEROS        ARBDPC
030300950130    2C                   END
030400000000     C*
030500000000     C* SE TROVATO COD.ANOMALIA SUI COLLI IMPOSTO IL FLAG SULLA BOLLA
030600950130    2C     WFAN          IFEQ      'S'
030700000000     C                   MOVEL     'S'           ARBFAN
030800950130    2C                   ENDIF
030900950328   1AC                   ENDIF
031000950328     C* DATA ARRIVO
031100950328    2C     WDAM          IFNE      *HIVAL
031200950328     C                   MOVEL     WDAM          ARBDAM
031300950328     C                   ELSE
031400950328     C                   MOVEL     *ZEROS        ARBDAM
031500950328    2C                   END
031600950918     C*
031700950918     C* SE BOLLA LOCALE E CI SONO TUTTE LE DATE DI ARRIVO
031800950918     C*  AGGIORNO CON L'ULTIMA DATA, LA DATA ARRIVO ULTIMO COLLO
031900070504   1AC     agbagb        IFEQ      'DT'
032000040514     C
032100040226     c
032200040226    2C     arbtfp        IFEQ      arbtfa
032300040226     c
032400970414    3C     WDAU          IFNE      *HIVAL
032500970414     C* SOMMO 1 ALLA DATA BORDERO' E PRENDO LA DATA PIU' ALTA
032600970414     C*  TRA I COLLI  E LA BOLLA
032700970414     C                   Z-ADD     ARBDBR        G02INV
032800970414     C                   MOVEL     '3'           G02ERR
032900970414     C                   CALL      'XSRDA8'
033000970414     C                   PARM                    WLBDAT
033100970414     C     G02TGI        ADD       1             GIOTGI
033200970414     C*
033300970414     C                   CALL      'XSRGI8'
033400970414     C                   PARM                    WGIDAT
033500970414     C*
033600970414     C                   Z-ADD     GIOINV        DATAP             8 0
033700970414     C**
033800970414    4C     DATAP         IFGT      WDAU
033900970414     C                   Z-ADD     DATAP         ARBDTI
034000970414     C                   ELSE
034100970414     C                   Z-ADD     WDAU          ARBDTI
034200970414    4C                   ENDIF
034300960229     C* LA DATA ARRIVO ULTIMO COLLO NON PUO' ESSERE INFERIORE
034400960229     C*  ALLA DATA SPEDIZIONE
034500970414    4C     ARBDTI        IFLT      ARBDSP
034600960229     C                   Z-ADD     ARBDSP        ARBDTI
034700970414    4C                   ENDIF
034800970924     C     ARBFBS        IFEQ      ' '
034900950922     C                   MOVEL     'S'           ARBFBS
035000970924     C                   ELSE
035100970924     C     ARBFBS        IFEQ      'V'
035200040505     C     ARBFBS        oreq      'P'
035300970924     C                   MOVEL     'E'           ARBFBS
035400970924     C                   ENDIF
035500970924     C                   ENDIF
035600970924     C**
035700970414   X3C                   ELSE
035800970414     C*
035900970414     C* NON C'E' LA DATA DI ARRIVO SU TUTTI I COLLI
036000970414     C                   CLEAR                   ARBDTI
036100970414    3C                   ENDIF
036200970414    2C                   ENDIF
036300950918   1AC                   ENDIF
036400950918     C*
036500950328     C* SE C'E' DATA ULTIMO COLLO E < DI QUELLA GIA' CALCOLATA, AZZERO
036600950328     C* PER FARLA RICALCOLARE
036700070504   1AC     agbagb        IFEQ      'AB'
036800950328     C                   CLEAR                   ARBDTI
036900950328     C                   CLEAR                   ARBHTI
037000950328     C                   ENDIF
037100950922     C* TOCCO BOLLA
037200070504     C     agbfbs        IFEQ      'S'
037300970924     C     ARBFBS        IFEQ      ' '
037400970924     C                   MOVEL     'S'           ARBFBS
037500970924     C                   ELSE
037600970924     C     ARBFBS        IFEQ      'V'
037700040505     C     ARBFBS        oreq      'P'
037800970924     C                   MOVEL     'E'           ARBFBS
037900970924     C                   ENDIF
038000970924     C                   ENDIF
038100950922     C                   ENDIF
038200950922     C* VOLUME CML
038300070504  1  C     agbfvc        IFEQ      'S'
038400070504     C     agbfpc        OREQ      'S'
038500990728  2  C     ARBFBS        IFEQ      ' '
038600070504  3  C     agbfvc        IFEQ      'S'
038700990728     C                   MOVEL     'V'           ARBFBS
038800990728 E3  C                   ENDIF
038900070504  3  C     agbfpc        IFEQ      'S'
039000990728  4  C     ARBFBS        IFEQ      ' '
039100990728     C                   MOVEL     'P'           ARBFBS
039200990728 X4  C                   ELSE
039300990728     C                   MOVEL     'E'           ARBFBS
039400990728 E4  C                   END
039500990728 E3  C                   END
039600990728 X2  C                   ELSE
039700990728  3  C     ARBFBS        IFEQ      'S'
039800970924     C                   MOVEL     'E'           ARBFBS
039900990728  E3 C                   ENDIF
040000990728  E2 C                   ENDIF
040100990728  E1 C                   ENDIF
040200950922     C*
040300000000     C                   UPDATE    FNARB000
040400040401     c
040500040401     c* Scrivo DTA solo se cambiata la data
040600040401     c                   if        wdtadam<>arbdam
040700030320      * Se è un tipo bolla da trasmettere in sede
040800030320     c                   Clear                   ds3a
040900111006     c                   Z-Add     1             Codut             1 0
041000030320     c                   Eval      Cod = '3A'
041100030320     c                   Clear                   Key
041200030320     c                   Movel     ArbCbo        Key
041300030320     c     kTab          Chain     Tabel00f
041400030320     c                   If        %Found(Tabel00f)
041500030320     c                   Movel     TblUni        ds3a
041600030320     c                   EndIf
041700030320If  2c                   If        §3aBsd = *Blanks
041800030206      * Scrivo/Aggiorno Fidta
041900030109     c                   Eval      wFidta = '0'
042000030108     c                   Eval      SavPoc = ArbLna
042100030108     c                   Eval      SavTrd = 'DAM'
042200030109     c     kFidta        Setll     Fidta01l
042300030109     c                   Do        *Hival
042400030109     c     kFidta        Reade     Fidta01l
042500030109      * Fine file
042600030109     c                   If        %Eof(Fidta01l)
042700030109     c                   Leave
042800030109     c                   EndIf
042900030109      * Già trasmesso
043000030109     c                   If        DtaFtr <> *Blanks
043100030109     c                   Iter
043200030109     c                   EndIf
043300030109      * Aggiorno
043400030109     c                   Eval      DtaDta = ArbDam
043500030108     c                   Update    Fidta000
043600030109     c                   Eval      wFidta = '1'
043700030109     c                   EndDo
043800030109      * Scrivo
043900030109     c                   If        wFidta = '0'
044000030108     c                   Clear                   Fidta000
044100030108     c                   Eval      DtaAas = ArbAas
044200030108     c                   Eval      DtaLnp = ArbLnp
044300030108     c                   Eval      DtaNrs = ArbNrs
044400030108     c                   Eval      DtaNsp = ArbNsp
044500030108     c                   Eval      DtaTrd = SavTrd
044600030108     c                   Eval      DtaPoc = SavPoc
044700030109     c                   Eval      DtaDta = ArbDam
044800030108     c                   Write     Fidta000
044900030108     c                   EndIf
045000030320    2c                   EndIf
045100040401    2c                   EndIf
045200030108
045300950130    1C                   ENDIF
045400000000     C*
045500000000     C                   ENDSR
045600950130     C**************************************************************************
045700950130     C* AGGIORNO BOLLE  TRANSITO
045800950130     C**************************************************************************
045900950130     C     AGGBTP        BEGSR
046000951026     C                   SETOFF                                       99
046100031127     c* Per il momento leggiamo se richieso l'aggiornamento di 1 bolla
046200031127     c*  transito, tutte le bolle transito per i vari p.o. perchè non
046300031127     c*  sono ancora in grado di sapere per quale p.o. transito è stato
046400031127     c*  richiesto l'aggiornamento
046500950130     C*
046600031127     C     KSAV          SETLL     FNBTP01L
046700031127     C     KSAV          READE     FNBTP01L                             9934
046800031127     c
046900031127    0c                   dow       not *in34 and not *in99
047000031127     C                   MOVEL     *HIVAL        WDPC
047100031127     C                   MOVEL     *HIVAL        WDET
047200031127     C                   MOVEL     *HIVAL        WDAM
047300031127     C                   Z-ADD     *ZERO         WDUT
047400031127     C                   Z-ADD     *ZERO         WDUC
047500040401     c                   eval      wdtadet=btpdet
047600040401     c                   eval      wdtadut=btpdut
047700031127     C                   CLEAR                   WFAN
047800031128     C                   CLEAR                   Wagv              1
047900031128     C                   CLEAR                   Wagp              1
048000031127     c
048100031127     C     Kbtp          SETLL     FNBTT11L
048200031127     C     Kbtp          READE     FNBTT11L                               34
048300950130     C*
048400950130    1C     *IN34         DOWEQ     '0'
048500950130     C*
048600950130     C* CAUSALE PER AGGIORNAMENTO DATE
048700070504    2C     agbagb        IFEQ      'DT'
048800031128     c* aggiorno il campo del volume e del peso sulla testata, se è presente
048900031128     c*  almeno un volume o peso. Infatti col fatto che ci possono essere +
049000031128     c*  bolle, magari una ce lo ha e le altre no!!
049100031128     c                   if        bttvuc>0
049200031128     c                   eval      wagv='S'
049300031128     c                   endif
049400031128     c                   if        bttpuc>0
049500031128     c                   eval      wagp='S'
049600031128     c                   endif
049700950130     C*
049800950130     C* DATA ENTRATA AL TRANSITO
049900950130    3C     BTTDET        IFGT      0
050000950130     C     BTTDET        ANDLT     WDET
050100950130     C                   MOVEL     BTTDET        WDET
050200950130    3C                   END
050300950130     C*
050400950130     C* DATA USCITA DAL TRANSITO
050500950130    3C     BTTDUT        IFGT      0
050600950130     C     BTTDUT        ANDGT     WDUT
050700950130     C                   MOVEL     BTTDUT        WDUT
050800950130    3C                   END
050900950130     C*
051000950130     C* DATA PARTENZA 1 COLLO
051100950130    3C     BTTDFV        IFGT      0
051200950130    4C     BTTDFV        IFLT      WDPC
051300950130     C                   MOVEL     BTTDFV        WDPC
051400950130    4C                   END
051500950130     C* DATA PARTENZA ULTIMO COLLO
051600950130    4C     BTTDFV        IFGT      WDUC
051700950130     C                   MOVEL     BTTDFV        WDUC
051800950130    4C                   END
051900950130     C*
052000950130   X3C                   ELSE
052100950130     C                   Z-ADD     *HIVAL        WDUC
052200950130    3C                   END
052300950130     C*
052400950130     C* DATA ARRIVO
052500950130    3C     BTTDAM        IFGT      0
052600950130     C     BTTDAM        ANDLT     WDAM
052700950130     C                   MOVEL     BTTDAM        WDAM
052800950130    3C                   END
052900950130    2C                   ENDIF
053000950130     C*
053100031127     C     Kbtp          READE     FNBTT11L                               34
053200950130    1C                   ENDDO
053300950130     C*
053400950130     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
053500950130     C*   TRANSITO E FILIALE TRANSITO
053600031127     C**   KSAV          CHAIN     FNBTP01L                           3099
053700950130     C*
053800031127    1C**   *IN30         IFEQ      *OFF
053900031127     C**   *IN99         ANDEQ     *OFF
054000950130     C* DATA ENTRATA AL TRANSITO
054100950130    2C     WDET          IFNE      *HIVAL
054200950130     C                   MOVEL     WDET          BTPDET
054300950130     C                   ELSE
054400950130     C                   MOVEL     *ZEROS        BTPDET
054500950130    2C                   END
054600950130     C* DATA USCITA DAL TRANSITO
054700950130     C                   Z-ADD     WDUT          BTPDUT
054800950130     C* DATA ARRIVO
054900950130    2C     WDAM          IFNE      *HIVAL
055000950130     C                   MOVEL     WDAM          BTPDAM
055100950130     C                   ELSE
055200950130     C                   MOVEL     *ZEROS        BTPDAM
055300950130    2C                   END
055400950130     C* DATA PARTENZA ULTIMO COLLO
055500950130    2C     WDUC          IFNE      *HIVAL
055600950130     C                   MOVEL     WDUC          BTPDUC
055700950130     C                   ELSE
055800950130     C                   MOVEL     *ZEROS        BTPDUC
055900950130    2C                   END
056000950130     C* DATA PARTENZA PRIMO COLLO
056100950130    2C     WDPC          IFNE      *HIVAL
056200950130     C                   MOVEL     WDPC          BTPDPC
056300950130     C                   ELSE
056400950130     C                   MOVEL     *ZEROS        BTPDPC
056500950130    2C                   END
056600950922     C* VOLUME CML
056700070504     C     agbfvc        IFEQ      'S'
056800031128     c     WAGV          andeq     'S'
056900950922     C                   Z-ADD     99999         BTPNCR
057000950922     C                   ENDIF
057100990728     C* PESO CML
057200070504     C     agbfpc        IFEQ      'S'
057300031128     c     WAGP          andeq     'S'
057400990728     C                   Z-ADD     99999         BTPNCP
057500990728     C                   ENDIF
057600950130     C*
057700950130     C                   UPDATE    FNBTP000
057800040401     c
057900030320      * Se è un tipo bolla da trasmettere in sede
058000030320     c                   Clear                   ds3a
058100030320     c                   Z-Add     1             Codut
058200030320     c                   Eval      Cod = '3A'
058300030320     c                   Clear                   Key
058400030320     c                   Movel     BtpCbo        Key
058500030320     c     kTab          Chain     Tabel00f
058600030320     c                   If        %Found(Tabel00f)
058700030320     c                   Movel     TblUni        ds3a
058800030320     c                   EndIf
058900030320If  2c                   If        §3aBsd = *Blanks
059000040401     c
059100040415      * Scrivo/Aggiorno Fidta
059200040415     c                   Eval      SavPoc = BtpFlp
059300040415     c
059400040401     c* Trasmetto DET  solo se cambiato
059500040401   2ac                   if        wdtadet<>btpdet
059600030206      * DET
059700030109     c                   Eval      wFidta = '0'
059800030108     c                   Eval      SavTrd = 'DET'
059900030109     c     kFidta        Setll     Fidta01l
060000030109     c                   Do        *Hival
060100030109     c     kFidta        Reade     Fidta01l
060200030109      * Fine file
060300030109     c                   If        %Eof(Fidta01l)
060400030109     c                   Leave
060500030109     c                   EndIf
060600030109      * Già trasmesso
060700030109     c                   If        DtaFtr <> *Blanks
060800030109     c                   Iter
060900030109     c                   EndIf
061000030109      * Aggiorno
061100030109     c                   Eval      DtaDta = BtpDet
061200030108     c                   Update    Fidta000
061300030109     c                   Eval      wFidta = '1'
061400030109     c                   EndDo
061500030109      * Scrivo
061600030109     c                   If        wFidta = '0'
061700030108     c                   Clear                   Fidta000
061800030108     c                   Eval      DtaAas = BtpAas
061900030108     c                   Eval      DtaLnp = BtpLnp
062000030108     c                   Eval      DtaNrs = BtpNrs
062100030108     c                   Eval      DtaNsp = BtpNsp
062200030108     c                   Eval      DtaTrd = SavTrd
062300030108     c                   Eval      DtaPoc = SavPoc
062400030109     c                   Eval      DtaDta = BtpDet
062500030108     c                   Write     Fidta000
062600030108     c                   EndIf
062700040401    2c                   EndIf
062800030108
062900040401      * DUT
063000040401     c
063100040401     c* Trasmetto DUT  solo se cambiato
063200040422   2ac***                if        wdtadut<>btpdut
063300030109     c                   Eval      wFidta = '0'
063400030108     c                   Eval      SavTrd = 'DUT'
063500030109     c     kFidta        Setll     Fidta01l
063600030109     c                   Do        *Hival
063700030109     c     kFidta        Reade     Fidta01l
063800030109      * Fine file
063900030109     c                   If        %Eof(Fidta01l)
064000030109     c                   Leave
064100030109     c                   EndIf
064200030109      * Già trasmesso
064300030109     c                   If        DtaFtr <> *Blanks
064400030109     c                   Iter
064500030109     c                   EndIf
064600030109      * Aggiorno
064700030109     c                   Eval      DtaDta = BtpDut
064800030108     c                   Update    Fidta000
064900030109     c                   Eval      wFidta = '1'
065000030109     c                   EndDo
065100030109      * Scrivo
065200030109     c                   If        wFidta = '0'
065300030108     c                   Clear                   Fidta000
065400030108     c                   Eval      DtaAas = BtpAas
065500030108     c                   Eval      DtaLnp = BtpLnp
065600030108     c                   Eval      DtaNrs = BtpNrs
065700030108     c                   Eval      DtaNsp = BtpNsp
065800030108     c                   Eval      DtaTrd = SavTrd
065900030108     c                   Eval      DtaPoc = SavPoc
066000030109     c                   Eval      DtaDta = BtpDut
066100030108     c                   Write     Fidta000
066200030108     c                   EndIf
066300040422   2ac***                EndIf
066400030320    2c                   EndIf
066500030108
066600031127    1C**                 ENDIF
066700031127     c
066800031127     C     KSAV          READE     FNBTP01L                             9934
066900031127    0c                   enddo
067000950130     C*
067100950130     C                   ENDSR
067200950130     C**************************************************************************
067300950130     C* AGGIORNO BOLLE  PARTENZA
067400950130     C**************************************************************************
067500950130     C     AGGBLP        BEGSR
067600111005     c
067700950130     C                   MOVEL     *HIVAL        WDPC
067800950926     C                   MOVEL     *HIVAL        WDSE
067900950130     C                   MOVEL     *HIVAL        WDET
068000950130     C                   MOVEL     *HIVAL        WDAM
068100950130     C                   Z-ADD     *ZERO         WDUT
068200950130     C                   Z-ADD     *ZERO         WDUC
068300041129     C                   MOVEA     *ZERO         Wnfv
068400951026     C                   SETOFF                                       99
068500950130     C*
068600950130     C     KSAV          SETLL     FNBLT01L
068700950130     C     KSAV          READE     FNBLT01L                               34
068800950130     C*
068900950130    1C     *IN34         DOWEQ     '0'
069000950130     C*
069100950130     C* CAUSALE PER AGGIORNAMENTO DATE
069200070504    2C     agbagb        IFEQ      'DT'
069300950130     C*
069400950130     C* DATA PARTENZA 1 COLLO
069500950130    3C     BLTDFV        IFGT      0
069600950130    4C     BLTDFV        IFLT      WDPC
069700950130     C                   MOVEL     BLTDFV        WDPC
069800950130    4C                   END
069900950130     C* DATA PARTENZA ULTIMO COLLO
070000950130    4C     BLTDFV        IFGT      WDUC
070100950130     C                   MOVEL     BLTDFV        WDUC
070200041129     C                   MOVEL     BLTnFV        Wnfv(1)
070300041129     c                   else
070400041129     c* memorizzo se stessa data ma <> foglio
070500041129     c                   if        bltdfv=wduc and bltdfv>0 and
070600041129     c                             bltnfv<>wnfv(1)
070700041129     c                   eval      wnfv(2)=bltnfv
070800041129     c                   endif
070900041129     c
071000950130    4C                   END
071100950130     C*
071200950130   X3C                   ELSE
071300950130     C                   Z-ADD     *HIVAL        WDUC
071400950130    3C                   END
071500950926     C* DATA SPUNTA ENTRATA
071600950926    3C     BLTDSE        IFGT      0
071700950926    4C     BLTDSE        ANDLT     WDSE
071800950926     C                   MOVEL     BLTDSE        WDSE
071900950926    4C                   ENDIF
072000950926     C* DATA ENTRATA TRANSITO
072100950926    3C     BLTDET        IFGT      0
072200950926    4C     BLTDET        ANDLT     WDET
072300950926     C                   MOVEL     BLTDET        WDET
072400950926    4C                   ENDIF
072500950926     C* DATA USCITA TRANSITO
072600950926    3C     BLTDUT        IFGT      0
072700950926    4C     BLTDUT        ANDLT     WDUT
072800950926     C                   MOVEL     BLTDUT        WDUT
072900950926    4C                   ENDIF
073000960520     C* DATA ARRIVO
073100960520    3C     BLTDAM        IFGT      0
073200960520    4C     BLTDAM        ANDLT     WDAM
073300960520     C                   MOVEL     BLTDAM        WDAM
073400960520    4C                   ENDIF
073500950130     C*
073600950130    2C                   ENDIF
073700950130     C*
073800950130     C     KSAV          READE     FNBLT01L                               34
073900950130    1C                   ENDDO
074000950130     C*
074100950130     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
074200950130     C*   TRANSITO E FILIALE TRANSITO
074300951026     C     KSAV          CHAIN     FNBLP01L                           3099
074400950130     C*
074500951026    1C     *IN30         IFEQ      *OFF
074600951026     C     *IN99         ANDEQ     *OFF
074700091021     c                   eval      wdtaduc=blpduc
074800950130     C* DATA PARTENZA ULTIMO COLLO
074900950130    2C     WDUC          IFNE      *HIVAL
075000950130     C                   MOVEL     WDUC          BLPDUC
075100041126     c* Se foglio borderizzazione <> da foglio aggiornamento DUC
075200041126     c*  e bolla già inviata, scrivo DTA per DUC
075300041129     c                   if        blpft1<>' '
075400041129     c                   if        (wnfv(1)<>blpnfv and wnfv(1)>0) or
075500041129     c                             (wnfv(2)<>blpnfv and wnfv(2)>0)
075600041129     c                   EXSR      SCRIVIduc
075700091021     c                   else
075800091021     c* Se il foglio è quello di borderizzazione ma la bolla non è più nel
075900091021     c*  fnblp46f, devo trasmettere lo stesso
076000091021     c     kblp46        chain     fnblp46l
076100091021     c                   if        not %found(fnblp46l)
076200091021     c                   EXSR      SCRIVIduc
076300041126     c                   endif
076400091021     c
076500091021     c                   endif
076600041129     c                   endif
076700041126     c
076800950130     C                   ELSE
076900950130     C                   MOVEL     *ZEROS        BLPDUC
077000950130    2C                   END
077100950130     C* DATA PARTENZA PRIMO COLLO
077200950130    2C     WDPC          IFNE      *HIVAL
077300950130     C                   MOVEL     WDPC          BLPDPC
077400950130     C                   ELSE
077500950130     C                   MOVEL     *ZEROS        BLPDPC
077600950130    2C                   END
077700950926     C* DATA SPUNTA ENTRATA
077800950926    2C     WDSE          IFNE      *HIVAL
077900950926     C                   MOVEL     WDSE          BLPDSE
078000950926     C                   ELSE
078100950926     C                   MOVEL     *ZEROS        BLPDSE
078200950926    2C                   END
078300950926     C* DATA ENTRATA TRANSITO
078400950926    2C     WDET          IFNE      *HIVAL
078500950926     C                   MOVEL     WDET          BLPDET
078600950926     C                   ELSE
078700950926     C                   MOVEL     *ZEROS        BLPDET
078800950926    2C                   END
078900950926     C* DATA USCITA TRANSITO
079000950926    2C     WDUT          IFNE      *HIVAL
079100950926     C                   MOVEL     WDUT          BLPDUT
079200950926     C                   ELSE
079300950926     C                   MOVEL     *ZEROS        BLPDUT
079400950926    2C                   END
079500960520     C* DATA ARRIVO
079600960520    2C     WDAM          IFNE      *HIVAL
079700960520     C                   MOVEL     WDAM          BLPDAM
079800960520     C                   ELSE
079900960520     C                   MOVEL     *ZEROS        BLPDAM
080000960520    2C                   END
080100951122     C* VOLUME CML
080200070504     C     agbfvc        IFEQ      'S'
080300951122     C                   Z-ADD     99999         BLPNCR
080400951122     C                   ENDIF
080500990728     C* PESO CML
080600070504     C     agbfpc        IFEQ      'S'
080700990728     C                   Z-ADD     99999         BLPNCP
080800990728     C                   ENDIF
080900950130     C*
081000950130     C                   UPDATE    FNBLP000
081100950130    1C                   ENDIF
081200950130     C*
081300950130     C                   ENDSR
081400950130     C**************************************************************************
081500950130     C* DELETO I RECORD IN FNAGB00F DOPO AVER AGGIORNATO
081600950130     C**************************************************************************
081700950130     C     DELET         BEGSR
081800040309     c                   clear                   werror
081900950130     C*
082000951026     C* NON CANCELLO SE HO TROVATO L'ERRORE
082100040309    1C     *IN99         IFEQ      *OFF
082200070507     c     fnagbnrr      chain(e)  fnagb00f
082300031016     C*
082400031016     C* Gestisco eventuali errori d allocazine record x i quali tramite il
082500031016     C* *PGM d utilità TRUL82R scelgo d inviare 1 MSG alla coda dell'operatore
082600040309    2C                   IF        %error
082700031016     C*
082800031016     C* Valorizzo la DS di procedura
082900031016     C                   CLEAR                   TRUL82DS
083000070507     c                   eval      ul82§rrn = fnagbnrr
083100031016     C                   EVAL      UL82§FIL = 'FNAGB00F'
083200031016     C                   EVAL      UL82§WIN = 'N'
083300031016     C                   EVAL      UL82§F5  = 'S'
083400031016     C*
083500031016     C* Effettuo la chiamata al *PGM d utilità
083600031016     C                   CALL(e)   'TRUL82R'
083700031016     C                   PARM                    TRUL82DS
083800031016     C*
083900040309     c*** Se trovato record allocato vado a fine pgm
084000040309     c                   movel     'E'           werror            1
084100040309     c                   goto      enddel
084200040309    2C                   ENDIF
084300950130     C*
084400070507     c                   if        %found(fnagb00f)
084500070507     C                   DELETE    FNAGBF00
084600070507     c                   endif
084700040309    3C                   ENDIF
084800040309     C     enddel        ENDSR
084900950130     C**************************************************************************
085000950130     C* ELABORO LE BOLLE E AGGIORNO
085100950130     C**************************************************************************
085200950130     C     ELAB          BEGSR
085300110701     c                   eval      $err='0'
085400950130     C*
085500950130     C                   SELECT
085600160422     c* verifico se la causale è tutta numerica
085700160422     c*  --> aggiornamento della zona sulla bolla
085800160422     c                   when      %check(digits:agbagb)=0
085900160422     c                   EXSR      AGG_zona
086000110701     C* BOLLE ARRIVI flag inoltro
086100110701     C     agbtbo        WHENEQ    'A'
086200110701     C     agbagb        andeq     'ST'
086300110701     C                   EXSR      A_arbfin
086400110701     C* BOLLE PARTENZE flag inoltro
086500110701     C     agbtbo        WHENEQ    'P'
086600110701     C     agbagb        andeq     'ST'
086700110701     C                   EXSR      A_blpfin
086800111007     C* BOLLE SEDE     flag inoltro
086900111007     C     agbtbo        WHENEQ    'S'
087000111007     C     agbagb        andeq     'ST'
087100111007     C                   EXSR      A_tasfin
087200150604     C* BOLLE PARTENZE cambia LNP Bolla per clienti in file FIBSP
087300150604     C     agbtbo        WHENEQ    'P'
087400150604     C     agbagb        andeq     'CB'
087500150604     C                   EXSR      P_blpLNPB
087600950130     C* BOLLE ARRIVI
087700070504     C     agbtbo        WHENEQ    'A'
087800950130     C                   EXSR      AGGARB
087900950130     C* BOLLE PARTENZE
088000070504     C     agbtbo        WHENEQ    'P'
088100950130     C                   EXSR      AGGBLP
088200950130     C* BOLLE TRANSITO
088300070504     C     agbTBO        WHENEQ    'T'
088400950130     C                   EXSR      AGGBTP
088500950130     C                   ENDSL
088600950130     C*
088700110701     C* DELETO I RECORD senon c'e' stato errore in allocaizone record
088800150608     c                   if        $err='0'
088900950130     C                   EXSR      DELET
089000110701     c                   endif
089100950130     C*
089200950130     C                   ENDSR
089300041104     C**************************************************************************
089400041104     C* ELABORA BOLLE PER AGGIORNARE IL PESO CML
089500041104     C**************************************************************************
089600041104     C     ELAAGP        BEGSR
089700041108     C     1             SETLL     FNAGP00f
089800041108     C                   READ      FNAGP00f
089900041108     C                   DOW       NOT %EOF(FNAGP00f)
090000041104     c
090100041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
090200041108     c*  non penso sarà mai attivabile
090300041108     c                   if        agptbo='P'
090400041104     C     KAgp          CHAIN     FNblp01L                           38
090500041104     c  n38              exsr      APPMOPP
090600070705     c                   if        d39err=*blanks  and o57err=*blanks
090700041108     c                   delete    fnagp000
090800041108     c                   endif
090900041108     c                   endif
091000041108     c
091100041108     C                   READ      FNAGP00f
091200041104     C                   ENDDO
091300111005     c
091400111005     c* Chiudo file  del pgm richiamato
091500111005     c                   clear                   fnlv39ds
091600111005     c                   eval      d39tla='C'
091700111005     C                   MOVEL     fnlv39ds      KPJBU
091800111005     c                   call      'FNLV39R'
091900111005     C                   PARM                    KPJBA
092000111005     c*
092100041104     C                   ENDSR
092200041108     C**************************************************************************
092300041108     C* ELABORA BOLLE PER AGGIORNARE IL volume cML
092400041108     C**************************************************************************
092500041108     C     ELAAGV        BEGSR
092600041108     C     1             SETLL     FNAGV00f
092700041108     C                   READ      FNAGV00f
092800041108     C                   DOW       NOT %EOF(FNAGV00f)
092900041108     c
093000041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
093100041108     c*  non penso sarà mai attivabile
093200041108     c                   if        agvtbo='P'
093300041108     C     KAgv          CHAIN     FNblp01L                           38
093400041108     c  n38              exsr      APPMOVP
093500070705     c                   if        d20err=*blanks  and o57err=*blanks
093600041108     c                   delete    fnagV000
093700041108     c                   endif
093800041108     c                   endif
093900041108     c
094000041108     C                   READ      FNAGV00f
094100041108     C                   ENDDO
094200111005     c
094300111005     c* Chiudo file  del pgm richiamato
094400111005     c                   clear                   fnlv20ds
094500111005     c                   eval      d20tla='C'
094600111005     C                   MOVEL     fnlv20ds      KPJBU
094700111005     c                   call      'FNLV20R'
094800111005     C                   PARM                    KPJBA
094900111005     c
095000041108     C                   ENDSR
095100041104     C*---------------------------------------------------------------*
095200041104     C* ROUTINE DI APPLICAZIONE MODIFICHE PESO A BOLLA PARTENZA
095300041104     C*---------------------------------------------------------------*
095400041104     C     APPMOPP       BEGSR
095500041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
095600041104     C                   Z-ADD     BLPPKF        OLDPKF
095700041104     C                   Z-ADD     BLPPKC        OLDPKC
095800041104     C                   Z-ADD     BLPNCP        OLDNCP
095900041108     C                   Z-ADD     BLPvlF        OLDvlf
096000041108     C                   Z-ADD     BLPvlc        OLDvlc
096100041108     C                   Z-ADD     BLPNCr        OLDNCr
096200070705     c                   movel     blpfvf        OLDfvf
096300041104     C*
096400041108     C                   CLEAR                   fnlv39ds
096500041104     C                   MOVEL     'P'           D39TBO
096600041108     C                   MOVEL     'C'           D39TPP
096700041104     C                   Z-ADD     BLPPKB        D39PLB
096800041104     C                   Z-ADD     BLPPKF        D39PLF
096900041108     C                   Z-ADD     agpPKC        D39PLU
097000041108     C                   Z-ADD     agpNCP        D39NCP
097100041104     C                   Z-ADD     BLPAAS        D39AAS
097200041104     C                   Z-ADD     BLPNRS        D39NRS
097300041104     C                   Z-ADD     BLPLNP        D39LNP
097400041104     C                   Z-ADD     BLPNSP        D39NSP
097500041104     C                   MOVEL     BLPCBO        D39CBO
097600041104     C                   Z-ADD     BLPLNA        D39LNA
097700041104     C                   Z-ADD     BLPNCL        D39NCL
097800041104     C                   Z-ADD     *date         D39DTE
097900041108     C                   MOVEL     fnlv39ds      KPJBU
098000041104     C                   CALL      'FNLV39R'
098100041104     C                   PARM                    KPJBA
098200041108     C                   MOVEL     KPJBU         fnlv39ds
098300041104     C*
098400041104     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
098500041104    3C     D39ERR        IFNE      '1'
098600041104     C                   MOVEL     D39NCP        BLPNCP
098700041104     C                   MOVEL     D39PLU        BLPPKC
098800041104     C                   MOVEL     D39PLF        BLPPKF
098900041104     C* AGGIORNO FILE TASSAZIONE PADRONCINI
099000041108     C                   CLEAR                   fnlv57ds
099100041104     C                   MOVEL     'P'           I57TBO
099200041104     C                   EXSR      AGGTAS
099300070705     c                   if        o57err<>' '
099400070705     C                   MOVEL     OLDNCp        blpNCp
099500070705     C                   MOVEL     OLDpkc        blppkc
099600070705     C                   MOVEL     OLDpkf        blppkf
099700070705     c                   endif
099800070705     c
099900041104     C                   UPDATE    FNBLP000
100000041104E   2C                   ENDIF
100100041104     C*
100200041104     C                   ENDSR
100300041108     C*---------------------------------------------------------------*
100400041108     C* ROUTINE DI APPLICAZIONE MODIFICHE volume A BOLLA PARTENZA
100500041108     C*---------------------------------------------------------------*
100600041108     C     APPMOVP       BEGSR
100700041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
100800041108     C                   Z-ADD     BLPPKF        OLDPKF
100900041108     C                   Z-ADD     BLPPKC        OLDPKC
101000041108     C                   Z-ADD     BLPNCP        OLDNCP
101100041108     C                   Z-ADD     BLPvlF        OLDvlf
101200041108     C                   Z-ADD     BLPvlc        OLDvlc
101300041108     C                   Z-ADD     BLPNCr        OLDNCr
101400070705     c                   movel     blpfvf        OLDfvf
101500041108     C*
101600041108     C                   CLEAR                   fnlv20ds
101700041108     C                   MOVEL     'P'           D20TBO
101800041108     C                   MOVEL     'C'           D20tvl
101900041108     C                   MOVEL     BLPFVB        D20FVB
102000041108     C                   Z-ADD     BLPVLB        D20VLB
102100041108     C                   MOVEL     BLPFVF        D20FVF
102200041108     C                   Z-ADD     BLPVLF        D20VLF
102300041108     C                   Z-ADD     agVVLC        D20VLU
102400041108     C                   Z-ADD     agVNCR        D20NCR
102500041108     C                   Z-ADD     BLPAAS        D20AAS
102600041108     C                   Z-ADD     BLPNRS        D20NRS
102700041108     C                   Z-ADD     BLPLNP        D20LNP
102800041108     C                   Z-ADD     BLPNSP        D20NSP
102900041108     C                   MOVEL     BLPCBO        D20CBO
103000041108     C                   Z-ADD     BLPLNA        D20LNA
103100041108     C                   Z-ADD     BLPNCL        D20NCL
103200041108     C                   Z-ADD     *date         D20DTE
103300041108     C                   MOVEL     fnlv20ds      KPJBU
103400041108     C                   CALL      'FNLV20R'
103500041108     C                   PARM                    KPJBA
103600041108     c                   parm      'N'           pwstor            1
103700041108     C                   MOVEL     KPJBU         fnlv20ds
103800041108     C*
103900041108     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
104000041108    3C     D20ERR        IFNE      '1'
104100041108     C                   MOVEL     D20NCR        BLPNCR
104200041108     C                   MOVEL     D20VLU        BLPVLC
104300041108     C                   MOVEL     D20FVF        BLPFVF
104400041108     C                   MOVEL     D20VLF        BLPVLF
104500041108     C* AGGIORNO FILE TASSAZIONE PADRONCINI
104600041108     C                   CLEAR                   fnlv57ds
104700041108     C                   MOVEL     'P'           I57TBO
104800041108     C                   EXSR      AGGTAS
104900070705     c                   if        o57err<>' '
105000070705     C                   MOVEL     OLDNCR        blpNCR
105100070705     C                   MOVEL     OLDvlc        blpVLC
105200070705     C                   MOVEL     OLDFVF        blpFVF
105300070705     C                   MOVEL     OLDVLF        blpVLF
105400070705     c                   endif
105500070705     c
105600041108     C                   UPDATE    FNBLP000
105700041108E   2C                   ENDIF
105800041108     C*
105900041108     C                   ENDSR
106000041104     C*---------------------------------------------------------------*
106100041104     C* AGGIORNA TASSAZIONE PADRONCINI
106200041104     C*---------------------------------------------------------------*
106300041104     C     AGGTAS        BEGSR
106400041104     C                   MOVEL     blpAAS        I57AAS
106500041104     C                   MOVEL     blpLNP        I57LNP
106600041104     C                   MOVEL     blpNRS        I57NRS
106700041104     C                   MOVEL     blpNSP        I57NSP
106800041104     C                   MOVEL     BLPDRT        I57DRT
106900041104     C                   MOVEL     BLPNRT        I57NRT
107000041104     C                   MOVEL     BLPFPP        I57FPP
107100041104     C                   MOVEL     BLPNCL        I57NCL
107200041104     C                   MOVEL     BLPPDR        I57PDR
107300041108     c* Campi nuovi modificati
107400041104     C                   MOVEL     BLPPKC        I57Pcn
107500041104     C                   MOVEL     BLPncp        I57npn
107600041104     C                   MOVEL     BLPPKf        I57Pfn
107700041108     C                   MOVEL     BLPVLC        I57VCN
107800041108     C                   MOVEL     BLPNCR        I57NCN
107900041108     C                   MOVEL     BLPVLF        I57VFN
108000041108     c* Campi della bolla prima dellamodifica
108100041104     C                   MOVEL     OLDPKC        I57pkc
108200041104     C                   MOVEL     OLDncp        I57ncp
108300041104     C                   MOVEL     OLDPKF        I57PKF
108400041108     C                   MOVEL     OLDVLC        I57VLC
108500041108     C                   MOVEL     OLDNCR        I57NCR
108600041108     C                   MOVEL     OLDVLF        I57VLF
108700041104     C                   MOVEL     'C'           I57TVL
108800041104     C                   CALL      'FNLV57R'
108900041104     C                   PARM                    KPJBA
109000041108     C                   PARM                    fnlv57ds
109100111005     c                   eval      ApriLV57='S'
109200041104     C                   ENDSR
109300041126     C*---------------------------------------------------------------*
109400041126     C* scrivi fidta00f per data partenza ultimo collo
109500041126     C*---------------------------------------------------------------*
109600041129     C     ScriviDUC     BEGSR
109700041129     c* Scrivo DTA solo se cambiata la data
109800041129    1c                   if        wdtaduc<>blpduc
109900041129      * Se è un tipo bolla da trasmettere in sede
110000041129     c                   Clear                   ds3a
110100041129     c                   Z-Add     1             Codut
110200041129     c                   Eval      Cod = '3A'
110300041129     c                   Clear                   Key
110400041129     c                   Movel     blpCbo        Key
110500041129     c     kTab          Chain     Tabel00f
110600041129     c                   If        %Found(Tabel00f)
110700041129     c                   Movel     TblUni        ds3a
110800041129     c                   EndIf
110900041129If  2c                   If        §3aBsd = *Blanks
111000041129      * Scrivo/Aggiorno Fidta
111100041129     c                   Eval      wFidta = '0'
111200041129     c                   Eval      SavPoc = blplnp
111300041129     c                   Eval      SavTrd = 'DUC'
111400041129     c     kFidta        Setll     Fidta01l
111500041129     c                   Do        *Hival
111600041129     c     kFidta        Reade     Fidta01l
111700041129      * Fine file
111800041129     c                   If        %Eof(Fidta01l)
111900041129     c                   Leave
112000041129     c                   EndIf
112100041129      * Già trasmesso
112200041129     c                   If        DtaFtr <> *Blanks
112300041129     c                   Iter
112400041129     c                   EndIf
112500041129      * Aggiorno
112600041129     c                   Eval      DtaDta = blpduc
112700041129     c                   Update    Fidta000
112800041129     c                   Eval      wFidta = '1'
112900041129     c                   EndDo
113000041129      * Scrivo
113100041129     c                   If        wFidta = '0'
113200041129     c                   Clear                   Fidta000
113300041129     c                   Eval      DtaAas = blpAas
113400041129     c                   Eval      DtaLnp = blpLnp
113500041129     c                   Eval      DtaNrs = blpNrs
113600041129     c                   Eval      DtaNsp = blpNsp
113700041129     c                   Eval      DtaTrd = SavTrd
113800041129     c                   Eval      DtaPoc = SavPoc
113900041129     c                   Eval      DtaDta = blpduc
114000041129     c                   Write     Fidta000
114100041129     c                   EndIf
114200041129    2c                   EndIf
114300041129    1c                   EndIf
114400041126     C                   ENDSR
114500110701     C**************************************************************************
114600110701     C* AGGIORNO flag Inoltro bolla arrivi
114700110701     C**************************************************************************
114800110701     C     A_arbfin      BEGSR
114900110701     C*
115000110701     C     KSAV          chain(e)  FNARB01L
115100110701     c                   if        %error
115200110701     c                   eval      $err='1'
115300110701     c                   leavesr
115400110701     c                   endif
115500110701     c
115600110706     c                   if        %found(fnarb01l)  and arbfin<>'I' and
115700110706     c                             arbfin<>'D'
115800110701     c                   eval      arbfin=agbfbs
115900110701     c                   update    fnarb000
116000110706     c                   else
116100110706     c                   unlock    fnarb01l
116200110701     c                   endif
116300110701     c
116400110701     C                   ENDSR
116500110701     C**************************************************************************
116600110701     C* AGGIORNO flag Inoltro bolla Partenza
116700110701     C**************************************************************************
116800110701     C     A_blpfin      BEGSR
116900110701     C*
117000110701     C     KSAV          chain(e)  FNBLP01L
117100110701     c                   if        %error
117200110701     c                   eval      $err='1'
117300110701     c                   leavesr
117400110701     c                   endif
117500110701     c
117600110706     c                   if        %found(fnblp01l) and blpfin<>'I' and
117700110706     c                             blpfin<>'D'
117800110701     c                   eval      BLPfin=agbfbs
117900110701     c                   update    fnblp000
118000110706     c                   else
118100110706     c                   unlock    fnblp01l
118200110701     c                   endif
118300110701     c
118400110701     C                   ENDSR
118500111007     C**************************************************************************
118600111007     C* AGGIORNO flag Inoltro bolla Sede
118700111007     C**************************************************************************
118800111007     C     A_tasfin      BEGSR
118900111007     C*
119000111007     c                   setoff                                       020304
119100130124     c                   clear                   wagg1
119200111007     C     KSAV          chain(e)  titas30c
119300111007     c
119400111011    1c                   if        %error or not %found(titas30c)
119500111007     c                   eval      $err='1'
119600111007     c                   leavesr
119700111011    1c                   endif
119800111007     c
119900111011    1c                   if        %found(titas30c)
120000111011
120100111011    2c                   if        *in02  and tasfic=' '  and tasfin<>'D'
120200111007     c                             and tasfin<>'I'
120300111007     c                   eval      tasfin=agbfbs
120400111007     c                   update    titas000
120500130124     c                   eval      wagg1='S'
120600111011     c
120700111011     c* cerco se c'e' seconda bolla
120800111011     c                   setoff                                       020304
120900111011     C     KSAV          reade(e)  titas30c
121000111011     c
121100111011    3c                   if        %error
121200111011     c                   eval      $err='1'
121300111011     c                   leavesr
121400111011    3c                   endif
121500111011     c                   if        not %eof(titas30c) and
121600111011    3c                             *in02  and tasfic=' '  and tasfin<>'D'
121700111011     c                             and tasfin<>'I'
121800111011     c                   eval      tasfin=agbfbs
121900111011     c                   update    titas000
122000111011    3c                   endif
122100111011    c
122200111011    2c                   endif
122300130124     c* Già contabilizzata - I bolla
122400130124     c* se inoltro non aggiornato perchè bolla già contabilizzata
122500130124     c* bisogna scrivere file per recupero variazioni post-fatturazione
122600130305     c* Non scrivo mai tivrb per la seconda bolla in quanto il flag
122700130305     c* inoltro non influisce sulla tassazione della seconda bolla
122800130305    2c                   if        wagg1=*blanks and tasfin<>'D' and
122900130305     c                                               tasfin<>'I'
123000130305     c                   exsr      sr_tivrb
123100130305    2c                   endif
123200111011    1c                   endif
123300111007
123400111007     c                   unlock    titas30c
123500111007     c
123600111007     C                   ENDSR
123700160422     C**************************************************************************
123800160422     C* AGGIORNO zona  bolla Partenza
123900160422     C**************************************************************************
124000160422     C     AGG_zona      BEGSR
124100160422     C*
124200160422     C     KSAV          chain(e)  FNBLP01L
124300160422     c                   if        %error
124400160422     c                   eval      $err='1'
124500160422     c                   leavesr
124600160422     c                   endif
124700160422     c
124800160422     c                   if        %found(fnblp01l)
124900160422     c                   movel     agbagb        blpznc
125000160422     c                   update    fnblp000
125100160422     c                   endif
125200160422
125300160422     c* Se fnblp trasmesso aggiorno anche FNARB
125400160505     c                   if        blpft1<>'N' and blpft1<>' '
125500160422     C     KSAV          chain(e)  FNARB01L
125600160422     c                   if        %error
125700160422     c                   eval      $err='1'
125800160422     c                   leavesr
125900160422     c                   endif
126000160422     c
126100160422     c                   if        %found(fnarb01l)
126200160422     c                   movel     agbagb        arbznc
126300160422     c                   update    fnarb000
126400160422     c                   endif
126500160422
126600160422     c                   endif
126700160422     c
126800160422     C                   ENDSR
126900150604     C**************************************************************************
127000150604     C* AGGIORNO lnp Bolla per collo di cliente che parte da altra filiale
127100150604     C**************************************************************************
127200150604     C     P_blpLNPB     BEGSR
127300150609     C     KSAV          chain(n)  FNBLP01L
127400150604     c* se la bolla manca è già stata probabilmente "presa" da un'altra filiale
127500150604     c* per cui non devo fare nulla
127600150605    1c                   if        %found(fnblp01l)
127700150605     c* se presente data entrata non devo fare niente
127800150605    2c                   if        blpdse>0
127900150604     c                   leavesr
128000150605    2c                   endif
128100150605     c                   exsr      sr_fibsp
128200150608     c                   if        wBspTp<>'K' and wBspTp<>'L'
128300150608     c                   leavesr
128400150608     c                   endif
128500150604     c* lettura delle spunte per determinare la nuova lnp bolla
128600150608     c                   clear                   wlnpb
128700150608     c                   move      *hival        wdcs
128800150608     c                   clear                   whcs
128900150608     c                   clear                   wdfv
129000150608     c* segnacolli non sequenziali
129100150608    2c                   if        blpfns='S'
129200150608     c     ksav          setll     fnblt01l
129300150608     c     ksav          reade     fnblt01l
129400150608    3c                   dow       not %eof(fnblt01l)
129500150608     c                   z-add     bltnsc        wncd
129600150608     c                   exsr      sr_spunte
129700150608     c     ksav          reade     fnblt01l
129800150608    3c                   enddo
129900150608     c* segnacolli sequenziali
130000150608   x2c                   else
130100150605     c                   eval      wncd=blpncd
130200150605     c                   eval      wnca=blpnca
130300150608    3c                   if        wnca=0
130400150605     c                   eval      wnca=blpncd
130500150608    3c                   endif
130600150608    3c                   dow       wncd<=wnca
130700150608     c                   exsr      sr_spunte
130800150605     c                   add       1             wncd
130900150608    3c                   enddo
131000150608    2c                   endif
131100150625    2c                   if        wBspTp='L'
131200150605     c     wlnpb         chain     azorg01l
131300150605     c                   movel     orgde3        og143
131400150625    3c                   if        §oglnpb>*zeros
131500150605     c                   move      §OGLNPB       wlnpb
131600150625   x3c                   else
131700150624     c* se su azorg la lnp valida come lnp bolla è vuota prendo da azcae la
131800150625     c* prima filiale dpd del terminal
131900150625     C                   CLEAR                   TRUL06DS
132000150625     C                   MOVE      'PP'          D06COD
132100150625     C                   MOVEL     wlnpb         D06KEY
132200150625     c                   move      'S'           d06esc
132300150625     C                   MOVEL     trul06ds      KPJBU
132400150625     c                   call      'TRUL06R'
132500150625     C                   PARM                    KPJBA
132600150625     c                   movel     kpjbu         trul06ds
132700150625     c* loop su schiera delle filiali
132800150625     c                   z-add     1             idx
132900150625     c                   clear                   wlnpb
133000150625     c                   dow       idx<=30  and l06(idx)>0 and wlnpb=0
133100150625     c     l06(idx)      chain     azorg01l
133200150625     c                   if        %found(azorg01l)
133300150625     c                   movel     orgde3        og143
133400150625    3c                   if        §ogntw='DPD'
133500150625     c                   move      l06(idx)      wlnpb
133600150625     c                   endif
133700150625     c                   endif
133800150625     c                   add       1             idx
133900150625     c                   enddo
134000150625     c
134100150625    3c                   endif
134200150624     c
134300150625    2c                   endif
134400150608     c                   if        wlnpb>0 and wlnpb<>blplnp
134500150605     c* richiamo pgm di modifica linea di partenza bolla su archivio bolle part
134600150605     c* e file collegati
134700150605     c                   clear                   fnls65ds
134800150605     c                   z-add     blpaas        I65AAS
134900150605     c                   z-add     blplnp        I65LNP
135000150605     c                   z-add     blpnrs        I65NRS
135100150605     c                   z-add     blpnsp        I65NSP
135200150605     c                   z-add     wlnpb         I65lnpn
135300150605     c                   movel     d53dfv        I65AASN
135400150605     c                   move      D53dfv        I65MGSN
135500150605     c                   movel     'S'           I65AGDRT
135600150605     c
135700150605     c                   move      '1'           I65STRCMT
135800150605     c                   move      '1'           I65FCMT
135900150605     c                   move      '1'           I65CMTRLBK
136000150605     c                   movel     fnls65ds      kpjbu
136100150605     c                   call      'FNLS65R'
136200150605     c                   parm                    kpjba
136300150605     c                   movel     kpjbu         fnls65ds
136400150824     c                   if        o65err='6' or
136500150824     c                             o65err='1' or
136600150824     c                             o65err='2' or
136700150824     c                             o65err='3'
136800150609     c                   eval      $err='1'
136900150609     c                   endif
137000150605     c                   endif
137100150608    1c                   endif
137200150604     c                   endsr
137300150608     C**************************************************************************
137400150608     C*  Lettura spunte per determinare la lnp bolla
137500150608     C**************************************************************************
137600150608     C     sr_spunte     BEGSR
137700150608     c     kbrv07        setll     fnbrv07l
137800150608     c     kbrv07        reade     fnbrv07l
137900150608    1c                   dow       not %eof(fnbrv07l)
138000150608     c* elaboro solo se  blptfp<>brvfle
138100150608     c* elaboro solo se la spunta è stata fatta da una delle filiali abilitate
138200150608    4c***                if        blptfp<>brvfle  and
138300150608    2c                   if        %lookup(brvpes:sbsp)>0
138400150608     c                             and (brvdcs<wdcs or (brvdcs=wdcs  and
138500150608     c                                  brvhcs<whcs))
138600150608     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
138700150608     c                   clear                   dslv53
138800150608     C                   z-add     brvnFV        D53NFV
138900150608     C                   MOVEL     brvNPG        D53NPG
139000150608     C                   MOVEL     brvFGS        D53FGS
139100150608     C                   CALL      'FNLV53R'
139200150608     C                   PARM                    DSLV53
139300150608     c* memorizzo
139400150608     c                   eval      wlnpb=brvpes
139500150608     c                   eval      wdcs=brvdcs
139600150608     c                   eval      whcs=brvhcs
139700150608     c                   eval      wdfv=d53dfv
139800150608    2c                   endif
139900150608     c     kbrv07        reade     fnbrv07l
140000150608    1c                   enddo
140100150608     c                   endsr
140200150605     C**************************************************************************
140300150605     C*
140400150605     C**************************************************************************
140500150605     C     sr_fibsp      BEGSR
140600150605     c                   eval      WBspTp=*blanks
140700150605    2c                   if        blpccm>0
140800150605     c                   eval      w_clibo=blpccm
140900150605     c                   else
141000150605     c                   eval      w_clibo=blpksc
141100150605    2c                   endif
141200150605     c     w_clibo       chain     fibsp02l
141300150608     c                   if        %found(fibsp02l) and bspfpa='S'
141400150608     c                   eval      wBspTp='K'
141500150608     c****               if        not %found(fibsp02l) or bspfpa<>'S'
141600150608     c                   else
141700150605     c     blplnp        chain     fibsp01l
141800150605     c                   if        %found(fibsp01l) and bspfpa='S'
141900150605     c                   eval      wBspTp='L'
142000150605     c                   endif
142100150605     c                   endif
142200150605     c                   endsr
142300130201     C**************************************************************************
142400130201     C* Richiamo pgm gestione file recupero variazioni post-fatturaz. (TIVRB)
142500130201     C**************************************************************************
142600130201     C     sr_tivrb      BEGSR
142700130201     c                   clear                   fnlvv1ds
142800130201     c                   eval      iv1cvb='ZT'
142900130201     c                   eval      iv1pgm=nompgm
143000130201     c                   eval      iv1aas=tasaas
143100130201     c                   eval      iv1lnp=taslnp
143200130201     c                   eval      iv1nrs=tasnrs
143300130201     c                   eval      iv1nsp=tasnsp
143400130201     c                   eval      iv1tbl=tastbl
143500130201     c                   clear                   parvar
143600130201     c                   movel     agbfbs        parvar
143700130204     c                   movel     fnlvv1ds      kpjbu
143800130206     c                   call      'FNLVV1R'
143900130204     c                   parm                    kpjba
144000130201     c                   parm                    parvar
144100130201     c                   endsr
144200000000     C**************************************************************************
144300000000     C* R O U T I N E    I N I Z I A L E
144400000000     C**************************************************************************
144500000000     C     *INZSR        BEGSR
144600950130     C*
144700000000     C     KART          KLIST
144800000000     C                   KFLD                    ARTAAS
144900000000     C                   KFLD                    ARTLNP
145000000000     C                   KFLD                    ARTNRS
145100000000     C                   KFLD                    ARTNSP
145200950130     C     KBTT          KLIST
145300950130     C                   KFLD                    BTTAAS
145400950130     C                   KFLD                    BTTLNP
145500950130     C                   KFLD                    BTTNRS
145600950130     C                   KFLD                    BTTNSP
145700031127     C     KBTP          KLIST
145800031127     C                   KFLD                    BTPflp
145900031127     C                   KFLD                    BTpAAS
146000031127     C                   KFLD                    BTpLNP
146100031127     C                   KFLD                    BTpNRS
146200031127     C                   KFLD                    BTpNSP
146300950130     C     KBLT          KLIST
146400950130     C                   KFLD                    BLTAAS
146500950130     C                   KFLD                    BLTLNP
146600950130     C                   KFLD                    BLTNRS
146700950130     C                   KFLD                    BLTNSP
146800041104     C     KAGp          KLIST
146900041104     C                   KFLD                    AGpAAS
147000041104     C                   KFLD                    AGpLNP
147100041104     C                   KFLD                    AGpNRS
147200041104     C                   KFLD                    AGpNSP
147300041108     C     KAGV          KLIST
147400041108     C                   KFLD                    AGvAAS
147500041108     C                   KFLD                    AGvLNP
147600041108     C                   KFLD                    AGvNRS
147700041108     C                   KFLD                    AGvNSP
147800000000     C     KSAV          KLIST
147900070504     C                   KFLD                    agbAAS
148000070504     C                   KFLD                    agbLNP
148100070504     C                   KFLD                    agbNRS
148200070504     C                   KFLD                    agbNSP
148300030108
148400030108     c     kFidta        Klist
148500070504     c                   Kfld                    agbAas
148600070504     c                   Kfld                    agbLnp
148700070504     c                   Kfld                    agbNrs
148800070504     c                   Kfld                    agbNsp
148900030108     c                   Kfld                    SavTrd
149000030108     c                   Kfld                    SavPoc
149100091021     C     Kblp46        KLIST
149200091021     C                   KFLD                    BlPtfp
149300091021     C                   KFLD                    BlpAAS
149400091021     C                   KFLD                    BlpLNP
149500091021     C                   KFLD                    BlpNRS
149600091021     C                   KFLD                    BlpNSP
149700150605     C     Kbrv07        KLIST
149800150605     C                   KFLD                    Blpfls
149900150605     C                   KFLD                    Blplna
150000150605     C                   KFLD                    Blpnrs
150100150605     C                   KFLD                    wncd
150200030320
150300030320     c     kTab          Klist
150400030320     c                   Kfld                    Codut
150500030320     c                   Kfld                    Cod
150600030320     c                   Kfld                    Key
150700000000     C*
150800950926     C     *LIKE         DEFINE    BLPDSE        WDSE
150900950926     C     *LIKE         DEFINE    ARBDET        WDET
151000950130     C     *LIKE         DEFINE    ARBDUT        WDUT
151100000000     C     *LIKE         DEFINE    ARTDAM        WDAM
151200950328     C     *LIKE         DEFINE    ARTDAM        WDAU
151300950130     C     *LIKE         DEFINE    ARBDPC        WDPC
151400950130     C     *LIKE         DEFINE    ARBDUC        WDUC
151500950130     C     *LIKE         DEFINE    ARBFAN        WFAN
151600000000     C*
151700000000     C                   ENDSR
