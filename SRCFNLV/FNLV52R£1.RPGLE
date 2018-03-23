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
002300030108
002400041129     D WNFV            S              5  0 DIM(2)
002500030108
002600041108     d oldpkc          s                   Like(blppkc)
002700041108     d oldncp          s                   Like(blpncp)
002800041108     d oldpkf          s                   Like(blppkf)
002900041108     d oldvlc          s                   Like(blpvlc)
003000041108     d oldncr          s                   Like(blpncr)
003100041108     d oldvlf          s                   Like(blpvlf)
003200070705     d oldFVF          s                   Like(blpFVF)
003300041108     d SavPoc          s                   Like(DtaPoc)
003400030108     d SavTrd          s                   Like(DtaTrd)
003500030109     d wFidta          s              1    Inz('0')
003600110701     d $err            s              1    Inz('0')
003700030320     d Cod             s                   Like(TblCod)
003800030320     d Key             s                   Like(TblKey)
003900040401     d wdtadam         s                   Like(Arbdam)
004000040401     d wdtadet         s                   Like(btpdet)
004100040401     d wdtadut         s                   Like(btpdut)
004200041126     d wdtaduc         s                   Like(blpduc)
004201130204     d wagg1           s              1
004203130206     d parvar          s            224
004300030108
004400070507     D fnagbinf        ds
004500070507     D  fnagbnrr             397    400i 0
004600960229     D                 DS
004700960229     D  ARBAAS                 1      4  0
004800960229     D  ARBMGS                 5      8  0
004900960229     D  ARBDSP                 1      8  0
005000970414     D WLBDAT          DS                  INZ
005100970414     D  G02DAT                 1      8  0
005200970414     D  G02INV                 9     16  0
005300970414     D  G02ERR                17     17
005400970414     D  G02TGI                18     22  0
005500970414     D WGIDAT          DS                  INZ
005600970414     D  GIODAT                 1      8  0
005700970414     D  GIOINV                 9     16  0
005800970414     D  GIOTGI                17     21  0
005900111006     d
006000950130     D* PARAMETRI DI PASSAGGIO
006100950130     D PARAM           DS
006200950130     D  PARTBO                 1      1
006300111006     d
006400950130     D KPJBA         E DS
006500031016      * DS Esterna x gestione allocazione ed invio messaggi
006600031016     D TRUL82DS      E DS
006700041108     D fnlv39ds      E DS
006800041108     D fnlv20ds      E DS
006900041108     D fnlv57ds      E DS
006901130204     D fnlvv1ds      E DS
007000030320
007100030320     d ds3a          e ds
007200110701     d
007300111007     d TAS30           s             21    inz('GAITRAGRPS/TITAS30C')
007301130124     D                SDS
007302130124     D  NOMPGM                 1     10
007400111007     d
007500111007     ITITAS000      02
007600111007     ITITAS010      03
007700111007     ITITASP00      04
007800950130     I*----------------------------------------------------------------
007900950130     C     *ENTRY        PLIST
008000950130     C                   PARM                    KPJBA
008100040514     C                   PARM                    chiudi            1
008200111006     c
008300040514     C                   SETOFF                                       30
008400111005     c                   clear                   ApriLV57          1
008500111007     c
008600111007     c* Se non aperto apro il file
008700111007     c                   if        not %open(titas30c)
008800111007     C                   OPEN(e)   titas30c
008900111007     c
009000111007     c                   if        not %open(titas30c)
009100111007     c                   eval      %subst(tas30:7:4)='GRU '
009200111007     C                   OPEN      titas30c
009300111007     c                   endif
009400111007     c                   endif
009500950130     C*
009600950130     C* LETTURA DEL FILE
009700970117    1C     PARTBO        IFEQ      ' '
009800070504     C     *LOVAL        SETLL     FNAGB02L
009900950130     C                   ELSE
010000070504     C     PARTBO        SETLL     FNAGB02L
010100970117    1C                   ENDIF
010200040315     c
010300040315    1c                   dow       not *in30
010400950130     C*
010500040315    2C     PARTBO        IFEQ      ' '
010600070507     C                   READ      FNAGB02L                               30
010700950130     C                   ELSE
010800070507     C     PARTBO        READE     FNAGB02L                               30
010900040315    2C                   ENDIF
011000950130     C*
011100111006     c* Escludo tipo bolla "S"
011200111010    2C                   if        not *in30
011300950130     C*
011400950130     C                   EXSR      ELAB
011500040309     c
011600070507    3c                   if        werror='E'
011700040309     c                   goto      fine
011800070507    3c                   endif
011900070507    2C                   ENDIF
012000950130     C*
012100040315    1C                   ENDDO
012200040315     c
012300041108     C
012400041108     c     fine          tag
012500040309     c
012600041104     C* ELABORA LE BOLLE PER IL PESO
012700041104     C                   EXSR      ELAAGP
012800041108     C* ELABORA LE BOLLE PER IL volume
012900041108     C                   EXSR      ELAAGV
013000111005
013100111005     c                   if        ApriLV57='S'
013200111005     c                   clear                   fnlv57ds
013300111005     c                   eval      i57tla='C'
013400111005     c                   call      'FNLV57R'
013500111005     C                   PARM                    KPJBA
013600111005     C                   PARM                    fnlv57ds
013700111005     c                   endif
013701130201     C                   MOVEL     'C'           iv1tla
013702130201     c                   movel     fnlvv1ds      kpjbu
013703130206     C                   CALL      'FNLVV1R'
013704130201     C                   PARM                    kpjba
013705130201     c
013800040514     C*
013900040514     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
014000040514     C                   SHTDN                                        98
014100040514     c
014200040514     c                   if        *in98
014300040514     c                   eval      chiudi='S'
014400111006     C****               SETON                                        LR
014500040514     c                   else
014600111005
014700111006     C****               SETON                                        RT
014800040514     c                   endif
014900111006     c
015000111006     C                   SETON                                        LR
015100950130     C*
015200950130     C**************************************************************************
015300950130     C* AGGIORNO BOLLE  ARRIVI
015400950130     C**************************************************************************
015500950130     C     AGGARB        BEGSR
015600950130     C                   MOVEL     *HIVAL        WDPC
015700950130     C                   MOVEL     *HIVAL        WDET
015800950130     C                   MOVEL     *HIVAL        WDAM
015900950130     C                   Z-ADD     *ZERO         WDUT
016000950130     C                   Z-ADD     *ZERO         WDUC
016100950328     C                   Z-ADD     *ZERO         WDAU
016200950122     C                   CLEAR                   WFAN
016300951026     C                   SETOFF                                       99
016400000000     C*
016500000000     C     KSAV          SETLL     FNART01L
016600000000     C     KSAV          READE     FNART01L                               34
016700000000     C*
016800950130    1C     *IN34         DOWEQ     '0'
016900950130     C*
017000950130     C* CAUSALE PER AGGIORNAMENTO DATE
017100070504    2C     agbagb        IFEQ      'DT'
017200000000     C*
017300950130     C* DATA ENTRATA AL TRANSITO
017400950130    3C     ARTDET        IFGT      0
017500950130     C     ARTDET        ANDLT     WDET
017600950130     C                   MOVEL     ARTDET        WDET
017700950130    3C                   END
017800000000     C*
017900000000     C* DATA USCITA DAL TRANSITO
018000950130    3C     ARTDUT        IFGT      0
018100950130     C     ARTDUT        ANDGT     WDUT
018200950130     C                   MOVEL     ARTDUT        WDUT
018300950130    3C                   END
018400950130     C*
018500950130     C* DATA PARTENZA 1 COLLO
018600950130    3C     ARTDFV        IFGT      0
018700950130    4C     ARTDFV        IFLT      WDPC
018800950130     C                   MOVEL     ARTDFV        WDPC
018900950130    4C                   END
019000950130     C* DATA PARTENZA ULTIMO COLLO
019100950130    4C     ARTDFV        IFGT      WDUC
019200950130     C                   MOVEL     ARTDFV        WDUC
019300950130    4C                   END
019400950130     C*
019500950130   X3C                   ELSE
019600950130     C                   Z-ADD     *HIVAL        WDUC
019700950130    3C                   END
019800950328     C* CODICE  ANOMALIA
019900950328    3C     ARTCAN        IFNE      *BLANKS
020000950328     C                   MOVEL     'S'           WFAN
020100950328    3C                   ENDIF
020200950328    2C                   END
020300950130     C*
020400950328     C* CAUSALE PER AGGIORNAMENTO DATE
020500070504    2C     agbagb        IFEQ      'DT'
020600950328     C* CAUSALE DI AGGIORNAMENTO DA ABBINAMENTO
020700070504    2C     agbagb        OREQ      'AB'
020800950328     C*
020900950328    3C     ARTDAM        IFGT      0
021000950328    4C     ARTDAM        IFLT      WDAM
021100950328     C                   MOVEL     ARTDAM        WDAM
021200950328    4C                   END
021300950918     C* DATA ARRIVO  ULTIMO COLLO LEGGENDO DA ARTDAM
021400950328    4C     ARTDAM        IFGT      WDAU
021500950328     C                   MOVEL     ARTDAM        WDAU
021600950328    4C                   END
021700950328     C*
021800950328   X3C                   ELSE
021900950328     C                   Z-ADD     *HIVAL        WDAU
022000950328    3C                   END
022100950130    2C                   ENDIF
022200000000     C*
022300000000     C     KSAV          READE     FNART01L                               34
022400950130    1C                   ENDDO
022500000000     C*
022600000000     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
022700000000     C*   TRANSITO E FILIALE TRANSITO
022800951026     C     KSAV          CHAIN     FNARB01L                           3099
022900000000     C*
023000951026    1C     *IN30         IFEQ      *OFF
023100951026     C     *IN99         ANDEQ     *OFF
023200040401     c                   z-add     arbdam        wdtadam
023300950328     C* CAUSALE PER AGGIORNAMENTO DATE
023400070504   1AC     agbagb        IFEQ      'DT'
023500950130     C* DATA ENTRATA AL TRANSITO
023600950130    2C     WDET          IFNE      *HIVAL
023700950130     C                   MOVEL     WDET          ARBDET
023800000000     C                   ELSE
023900000000     C                   MOVEL     *ZEROS        ARBDET
024000950130    2C                   END
024100000000     C* DATA USCITA DAL TRANSITO
024200950130     C                   Z-ADD     WDUT          ARBDUT
024300950130     C* DATA PARTENZA ULTIMO COLLO
024400950130    2C     WDUC          IFNE      *HIVAL
024500950130     C                   MOVEL     WDUC          ARBDUC
024600950130     C                   ELSE
024700950130     C                   MOVEL     *ZEROS        ARBDUC
024800950130    2C                   END
024900950130     C* DATA PARTENZA PRIMO COLLO
025000950130    2C     WDPC          IFNE      *HIVAL
025100950130     C                   MOVEL     WDPC          ARBDPC
025200950130     C                   ELSE
025300950130     C                   MOVEL     *ZEROS        ARBDPC
025400950130    2C                   END
025500000000     C*
025600000000     C* SE TROVATO COD.ANOMALIA SUI COLLI IMPOSTO IL FLAG SULLA BOLLA
025700950130    2C     WFAN          IFEQ      'S'
025800000000     C                   MOVEL     'S'           ARBFAN
025900950130    2C                   ENDIF
026000950328   1AC                   ENDIF
026100950328     C* DATA ARRIVO
026200950328    2C     WDAM          IFNE      *HIVAL
026300950328     C                   MOVEL     WDAM          ARBDAM
026400950328     C                   ELSE
026500950328     C                   MOVEL     *ZEROS        ARBDAM
026600950328    2C                   END
026700950918     C*
026800950918     C* SE BOLLA LOCALE E CI SONO TUTTE LE DATE DI ARRIVO
026900950918     C*  AGGIORNO CON L'ULTIMA DATA, LA DATA ARRIVO ULTIMO COLLO
027000070504   1AC     agbagb        IFEQ      'DT'
027100040514     C
027200040226     c
027300040226    2C     arbtfp        IFEQ      arbtfa
027400040226     c
027500970414    3C     WDAU          IFNE      *HIVAL
027600970414     C* SOMMO 1 ALLA DATA BORDERO' E PRENDO LA DATA PIU' ALTA
027700970414     C*  TRA I COLLI  E LA BOLLA
027800970414     C                   Z-ADD     ARBDBR        G02INV
027900970414     C                   MOVEL     '3'           G02ERR
028000970414     C                   CALL      'XSRDA8'
028100970414     C                   PARM                    WLBDAT
028200970414     C     G02TGI        ADD       1             GIOTGI
028300970414     C*
028400970414     C                   CALL      'XSRGI8'
028500970414     C                   PARM                    WGIDAT
028600970414     C*
028700970414     C                   Z-ADD     GIOINV        DATAP             8 0
028800970414     C**
028900970414    4C     DATAP         IFGT      WDAU
029000970414     C                   Z-ADD     DATAP         ARBDTI
029100970414     C                   ELSE
029200970414     C                   Z-ADD     WDAU          ARBDTI
029300970414    4C                   ENDIF
029400960229     C* LA DATA ARRIVO ULTIMO COLLO NON PUO' ESSERE INFERIORE
029500960229     C*  ALLA DATA SPEDIZIONE
029600970414    4C     ARBDTI        IFLT      ARBDSP
029700960229     C                   Z-ADD     ARBDSP        ARBDTI
029800970414    4C                   ENDIF
029900970924     C     ARBFBS        IFEQ      ' '
030000950922     C                   MOVEL     'S'           ARBFBS
030100970924     C                   ELSE
030200970924     C     ARBFBS        IFEQ      'V'
030300040505     C     ARBFBS        oreq      'P'
030400970924     C                   MOVEL     'E'           ARBFBS
030500970924     C                   ENDIF
030600970924     C                   ENDIF
030700970924     C**
030800970414   X3C                   ELSE
030900970414     C*
031000970414     C* NON C'E' LA DATA DI ARRIVO SU TUTTI I COLLI
031100970414     C                   CLEAR                   ARBDTI
031200970414    3C                   ENDIF
031300970414    2C                   ENDIF
031400950918   1AC                   ENDIF
031500950918     C*
031600950328     C* SE C'E' DATA ULTIMO COLLO E < DI QUELLA GIA' CALCOLATA, AZZERO
031700950328     C* PER FARLA RICALCOLARE
031800070504   1AC     agbagb        IFEQ      'AB'
031900950328     C                   CLEAR                   ARBDTI
032000950328     C                   CLEAR                   ARBHTI
032100950328     C                   ENDIF
032200950922     C* TOCCO BOLLA
032300070504     C     agbfbs        IFEQ      'S'
032400970924     C     ARBFBS        IFEQ      ' '
032500970924     C                   MOVEL     'S'           ARBFBS
032600970924     C                   ELSE
032700970924     C     ARBFBS        IFEQ      'V'
032800040505     C     ARBFBS        oreq      'P'
032900970924     C                   MOVEL     'E'           ARBFBS
033000970924     C                   ENDIF
033100970924     C                   ENDIF
033200950922     C                   ENDIF
033300950922     C* VOLUME CML
033400070504  1  C     agbfvc        IFEQ      'S'
033500070504     C     agbfpc        OREQ      'S'
033600990728  2  C     ARBFBS        IFEQ      ' '
033700070504  3  C     agbfvc        IFEQ      'S'
033800990728     C                   MOVEL     'V'           ARBFBS
033900990728 E3  C                   ENDIF
034000070504  3  C     agbfpc        IFEQ      'S'
034100990728  4  C     ARBFBS        IFEQ      ' '
034200990728     C                   MOVEL     'P'           ARBFBS
034300990728 X4  C                   ELSE
034400990728     C                   MOVEL     'E'           ARBFBS
034500990728 E4  C                   END
034600990728 E3  C                   END
034700990728 X2  C                   ELSE
034800990728  3  C     ARBFBS        IFEQ      'S'
034900970924     C                   MOVEL     'E'           ARBFBS
035000990728  E3 C                   ENDIF
035100990728  E2 C                   ENDIF
035200990728  E1 C                   ENDIF
035300950922     C*
035400000000     C                   UPDATE    FNARB000
035500040401     c
035600040401     c* Scrivo DTA solo se cambiata la data
035700040401     c                   if        wdtadam<>arbdam
035800030320      * Se è un tipo bolla da trasmettere in sede
035900030320     c                   Clear                   ds3a
036000111006     c                   Z-Add     1             Codut             1 0
036100030320     c                   Eval      Cod = '3A'
036200030320     c                   Clear                   Key
036300030320     c                   Movel     ArbCbo        Key
036400030320     c     kTab          Chain     Tabel00f
036500030320     c                   If        %Found(Tabel00f)
036600030320     c                   Movel     TblUni        ds3a
036700030320     c                   EndIf
036800030320If  2c                   If        §3aBsd = *Blanks
036900030206      * Scrivo/Aggiorno Fidta
037000030109     c                   Eval      wFidta = '0'
037100030108     c                   Eval      SavPoc = ArbLna
037200030108     c                   Eval      SavTrd = 'DAM'
037300030109     c     kFidta        Setll     Fidta01l
037400030109     c                   Do        *Hival
037500030109     c     kFidta        Reade     Fidta01l
037600030109      * Fine file
037700030109     c                   If        %Eof(Fidta01l)
037800030109     c                   Leave
037900030109     c                   EndIf
038000030109      * Già trasmesso
038100030109     c                   If        DtaFtr <> *Blanks
038200030109     c                   Iter
038300030109     c                   EndIf
038400030109      * Aggiorno
038500030109     c                   Eval      DtaDta = ArbDam
038600030108     c                   Update    Fidta000
038700030109     c                   Eval      wFidta = '1'
038800030109     c                   EndDo
038900030109      * Scrivo
039000030109     c                   If        wFidta = '0'
039100030108     c                   Clear                   Fidta000
039200030108     c                   Eval      DtaAas = ArbAas
039300030108     c                   Eval      DtaLnp = ArbLnp
039400030108     c                   Eval      DtaNrs = ArbNrs
039500030108     c                   Eval      DtaNsp = ArbNsp
039600030108     c                   Eval      DtaTrd = SavTrd
039700030108     c                   Eval      DtaPoc = SavPoc
039800030109     c                   Eval      DtaDta = ArbDam
039900030108     c                   Write     Fidta000
040000030108     c                   EndIf
040100030320    2c                   EndIf
040200040401    2c                   EndIf
040300030108
040400950130    1C                   ENDIF
040500000000     C*
040600000000     C                   ENDSR
040700950130     C**************************************************************************
040800950130     C* AGGIORNO BOLLE  TRANSITO
040900950130     C**************************************************************************
041000950130     C     AGGBTP        BEGSR
041100951026     C                   SETOFF                                       99
041200031127     c* Per il momento leggiamo se richieso l'aggiornamento di 1 bolla
041300031127     c*  transito, tutte le bolle transito per i vari p.o. perchè non
041400031127     c*  sono ancora in grado di sapere per quale p.o. transito è stato
041500031127     c*  richiesto l'aggiornamento
041600950130     C*
041700031127     C     KSAV          SETLL     FNBTP01L
041800031127     C     KSAV          READE     FNBTP01L                             9934
041900031127     c
042000031127    0c                   dow       not *in34 and not *in99
042100031127     C                   MOVEL     *HIVAL        WDPC
042200031127     C                   MOVEL     *HIVAL        WDET
042300031127     C                   MOVEL     *HIVAL        WDAM
042400031127     C                   Z-ADD     *ZERO         WDUT
042500031127     C                   Z-ADD     *ZERO         WDUC
042600040401     c                   eval      wdtadet=btpdet
042700040401     c                   eval      wdtadut=btpdut
042800031127     C                   CLEAR                   WFAN
042900031128     C                   CLEAR                   Wagv              1
043000031128     C                   CLEAR                   Wagp              1
043100031127     c
043200031127     C     Kbtp          SETLL     FNBTT11L
043300031127     C     Kbtp          READE     FNBTT11L                               34
043400950130     C*
043500950130    1C     *IN34         DOWEQ     '0'
043600950130     C*
043700950130     C* CAUSALE PER AGGIORNAMENTO DATE
043800070504    2C     agbagb        IFEQ      'DT'
043900031128     c* aggiorno il campo del volume e del peso sulla testata, se è presente
044000031128     c*  almeno un volume o peso. Infatti col fatto che ci possono essere +
044100031128     c*  bolle, magari una ce lo ha e le altre no!!
044200031128     c                   if        bttvuc>0
044300031128     c                   eval      wagv='S'
044400031128     c                   endif
044500031128     c                   if        bttpuc>0
044600031128     c                   eval      wagp='S'
044700031128     c                   endif
044800950130     C*
044900950130     C* DATA ENTRATA AL TRANSITO
045000950130    3C     BTTDET        IFGT      0
045100950130     C     BTTDET        ANDLT     WDET
045200950130     C                   MOVEL     BTTDET        WDET
045300950130    3C                   END
045400950130     C*
045500950130     C* DATA USCITA DAL TRANSITO
045600950130    3C     BTTDUT        IFGT      0
045700950130     C     BTTDUT        ANDGT     WDUT
045800950130     C                   MOVEL     BTTDUT        WDUT
045900950130    3C                   END
046000950130     C*
046100950130     C* DATA PARTENZA 1 COLLO
046200950130    3C     BTTDFV        IFGT      0
046300950130    4C     BTTDFV        IFLT      WDPC
046400950130     C                   MOVEL     BTTDFV        WDPC
046500950130    4C                   END
046600950130     C* DATA PARTENZA ULTIMO COLLO
046700950130    4C     BTTDFV        IFGT      WDUC
046800950130     C                   MOVEL     BTTDFV        WDUC
046900950130    4C                   END
047000950130     C*
047100950130   X3C                   ELSE
047200950130     C                   Z-ADD     *HIVAL        WDUC
047300950130    3C                   END
047400950130     C*
047500950130     C* DATA ARRIVO
047600950130    3C     BTTDAM        IFGT      0
047700950130     C     BTTDAM        ANDLT     WDAM
047800950130     C                   MOVEL     BTTDAM        WDAM
047900950130    3C                   END
048000950130    2C                   ENDIF
048100950130     C*
048200031127     C     Kbtp          READE     FNBTT11L                               34
048300950130    1C                   ENDDO
048400950130     C*
048500950130     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
048600950130     C*   TRANSITO E FILIALE TRANSITO
048700031127     C**   KSAV          CHAIN     FNBTP01L                           3099
048800950130     C*
048900031127    1C**   *IN30         IFEQ      *OFF
049000031127     C**   *IN99         ANDEQ     *OFF
049100950130     C* DATA ENTRATA AL TRANSITO
049200950130    2C     WDET          IFNE      *HIVAL
049300950130     C                   MOVEL     WDET          BTPDET
049400950130     C                   ELSE
049500950130     C                   MOVEL     *ZEROS        BTPDET
049600950130    2C                   END
049700950130     C* DATA USCITA DAL TRANSITO
049800950130     C                   Z-ADD     WDUT          BTPDUT
049900950130     C* DATA ARRIVO
050000950130    2C     WDAM          IFNE      *HIVAL
050100950130     C                   MOVEL     WDAM          BTPDAM
050200950130     C                   ELSE
050300950130     C                   MOVEL     *ZEROS        BTPDAM
050400950130    2C                   END
050500950130     C* DATA PARTENZA ULTIMO COLLO
050600950130    2C     WDUC          IFNE      *HIVAL
050700950130     C                   MOVEL     WDUC          BTPDUC
050800950130     C                   ELSE
050900950130     C                   MOVEL     *ZEROS        BTPDUC
051000950130    2C                   END
051100950130     C* DATA PARTENZA PRIMO COLLO
051200950130    2C     WDPC          IFNE      *HIVAL
051300950130     C                   MOVEL     WDPC          BTPDPC
051400950130     C                   ELSE
051500950130     C                   MOVEL     *ZEROS        BTPDPC
051600950130    2C                   END
051700950922     C* VOLUME CML
051800070504     C     agbfvc        IFEQ      'S'
051900031128     c     WAGV          andeq     'S'
052000950922     C                   Z-ADD     99999         BTPNCR
052100950922     C                   ENDIF
052200990728     C* PESO CML
052300070504     C     agbfpc        IFEQ      'S'
052400031128     c     WAGP          andeq     'S'
052500990728     C                   Z-ADD     99999         BTPNCP
052600990728     C                   ENDIF
052700950130     C*
052800950130     C                   UPDATE    FNBTP000
052900040401     c
053000030320      * Se è un tipo bolla da trasmettere in sede
053100030320     c                   Clear                   ds3a
053200030320     c                   Z-Add     1             Codut
053300030320     c                   Eval      Cod = '3A'
053400030320     c                   Clear                   Key
053500030320     c                   Movel     BtpCbo        Key
053600030320     c     kTab          Chain     Tabel00f
053700030320     c                   If        %Found(Tabel00f)
053800030320     c                   Movel     TblUni        ds3a
053900030320     c                   EndIf
054000030320If  2c                   If        §3aBsd = *Blanks
054100040401     c
054200040415      * Scrivo/Aggiorno Fidta
054300040415     c                   Eval      SavPoc = BtpFlp
054400040415     c
054500040401     c* Trasmetto DET  solo se cambiato
054600040401   2ac                   if        wdtadet<>btpdet
054700030206      * DET
054800030109     c                   Eval      wFidta = '0'
054900030108     c                   Eval      SavTrd = 'DET'
055000030109     c     kFidta        Setll     Fidta01l
055100030109     c                   Do        *Hival
055200030109     c     kFidta        Reade     Fidta01l
055300030109      * Fine file
055400030109     c                   If        %Eof(Fidta01l)
055500030109     c                   Leave
055600030109     c                   EndIf
055700030109      * Già trasmesso
055800030109     c                   If        DtaFtr <> *Blanks
055900030109     c                   Iter
056000030109     c                   EndIf
056100030109      * Aggiorno
056200030109     c                   Eval      DtaDta = BtpDet
056300030108     c                   Update    Fidta000
056400030109     c                   Eval      wFidta = '1'
056500030109     c                   EndDo
056600030109      * Scrivo
056700030109     c                   If        wFidta = '0'
056800030108     c                   Clear                   Fidta000
056900030108     c                   Eval      DtaAas = BtpAas
057000030108     c                   Eval      DtaLnp = BtpLnp
057100030108     c                   Eval      DtaNrs = BtpNrs
057200030108     c                   Eval      DtaNsp = BtpNsp
057300030108     c                   Eval      DtaTrd = SavTrd
057400030108     c                   Eval      DtaPoc = SavPoc
057500030109     c                   Eval      DtaDta = BtpDet
057600030108     c                   Write     Fidta000
057700030108     c                   EndIf
057800040401    2c                   EndIf
057900030108
058000040401      * DUT
058100040401     c
058200040401     c* Trasmetto DUT  solo se cambiato
058300040422   2ac***                if        wdtadut<>btpdut
058400030109     c                   Eval      wFidta = '0'
058500030108     c                   Eval      SavTrd = 'DUT'
058600030109     c     kFidta        Setll     Fidta01l
058700030109     c                   Do        *Hival
058800030109     c     kFidta        Reade     Fidta01l
058900030109      * Fine file
059000030109     c                   If        %Eof(Fidta01l)
059100030109     c                   Leave
059200030109     c                   EndIf
059300030109      * Già trasmesso
059400030109     c                   If        DtaFtr <> *Blanks
059500030109     c                   Iter
059600030109     c                   EndIf
059700030109      * Aggiorno
059800030109     c                   Eval      DtaDta = BtpDut
059900030108     c                   Update    Fidta000
060000030109     c                   Eval      wFidta = '1'
060100030109     c                   EndDo
060200030109      * Scrivo
060300030109     c                   If        wFidta = '0'
060400030108     c                   Clear                   Fidta000
060500030108     c                   Eval      DtaAas = BtpAas
060600030108     c                   Eval      DtaLnp = BtpLnp
060700030108     c                   Eval      DtaNrs = BtpNrs
060800030108     c                   Eval      DtaNsp = BtpNsp
060900030108     c                   Eval      DtaTrd = SavTrd
061000030108     c                   Eval      DtaPoc = SavPoc
061100030109     c                   Eval      DtaDta = BtpDut
061200030108     c                   Write     Fidta000
061300030108     c                   EndIf
061400040422   2ac***                EndIf
061500030320    2c                   EndIf
061600030108
061700031127    1C**                 ENDIF
061800031127     c
061900031127     C     KSAV          READE     FNBTP01L                             9934
062000031127    0c                   enddo
062100950130     C*
062200950130     C                   ENDSR
062300950130     C**************************************************************************
062400950130     C* AGGIORNO BOLLE  PARTENZA
062500950130     C**************************************************************************
062600950130     C     AGGBLP        BEGSR
062700111005     c
062800950130     C                   MOVEL     *HIVAL        WDPC
062900950926     C                   MOVEL     *HIVAL        WDSE
063000950130     C                   MOVEL     *HIVAL        WDET
063100950130     C                   MOVEL     *HIVAL        WDAM
063200950130     C                   Z-ADD     *ZERO         WDUT
063300950130     C                   Z-ADD     *ZERO         WDUC
063400041129     C                   MOVEA     *ZERO         Wnfv
063500951026     C                   SETOFF                                       99
063600950130     C*
063700950130     C     KSAV          SETLL     FNBLT01L
063800950130     C     KSAV          READE     FNBLT01L                               34
063900950130     C*
064000950130    1C     *IN34         DOWEQ     '0'
064100950130     C*
064200950130     C* CAUSALE PER AGGIORNAMENTO DATE
064300070504    2C     agbagb        IFEQ      'DT'
064400950130     C*
064500950130     C* DATA PARTENZA 1 COLLO
064600950130    3C     BLTDFV        IFGT      0
064700950130    4C     BLTDFV        IFLT      WDPC
064800950130     C                   MOVEL     BLTDFV        WDPC
064900950130    4C                   END
065000950130     C* DATA PARTENZA ULTIMO COLLO
065100950130    4C     BLTDFV        IFGT      WDUC
065200950130     C                   MOVEL     BLTDFV        WDUC
065300041129     C                   MOVEL     BLTnFV        Wnfv(1)
065400041129     c                   else
065500041129     c* memorizzo se stessa data ma <> foglio
065600041129     c                   if        bltdfv=wduc and bltdfv>0 and
065700041129     c                             bltnfv<>wnfv(1)
065800041129     c                   eval      wnfv(2)=bltnfv
065900041129     c                   endif
066000041129     c
066100950130    4C                   END
066200950130     C*
066300950130   X3C                   ELSE
066400950130     C                   Z-ADD     *HIVAL        WDUC
066500950130    3C                   END
066600950926     C* DATA SPUNTA ENTRATA
066700950926    3C     BLTDSE        IFGT      0
066800950926    4C     BLTDSE        ANDLT     WDSE
066900950926     C                   MOVEL     BLTDSE        WDSE
067000950926    4C                   ENDIF
067100950926     C* DATA ENTRATA TRANSITO
067200950926    3C     BLTDET        IFGT      0
067300950926    4C     BLTDET        ANDLT     WDET
067400950926     C                   MOVEL     BLTDET        WDET
067500950926    4C                   ENDIF
067600950926     C* DATA USCITA TRANSITO
067700950926    3C     BLTDUT        IFGT      0
067800950926    4C     BLTDUT        ANDLT     WDUT
067900950926     C                   MOVEL     BLTDUT        WDUT
068000950926    4C                   ENDIF
068100960520     C* DATA ARRIVO
068200960520    3C     BLTDAM        IFGT      0
068300960520    4C     BLTDAM        ANDLT     WDAM
068400960520     C                   MOVEL     BLTDAM        WDAM
068500960520    4C                   ENDIF
068600950130     C*
068700950130    2C                   ENDIF
068800950130     C*
068900950130     C     KSAV          READE     FNBLT01L                               34
069000950130    1C                   ENDDO
069100950130     C*
069200950130     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
069300950130     C*   TRANSITO E FILIALE TRANSITO
069400951026     C     KSAV          CHAIN     FNBLP01L                           3099
069500950130     C*
069600951026    1C     *IN30         IFEQ      *OFF
069700951026     C     *IN99         ANDEQ     *OFF
069800091021     c                   eval      wdtaduc=blpduc
069900950130     C* DATA PARTENZA ULTIMO COLLO
070000950130    2C     WDUC          IFNE      *HIVAL
070100950130     C                   MOVEL     WDUC          BLPDUC
070200041126     c* Se foglio borderizzazione <> da foglio aggiornamento DUC
070300041126     c*  e bolla già inviata, scrivo DTA per DUC
070400041129     c                   if        blpft1<>' '
070500041129     c                   if        (wnfv(1)<>blpnfv and wnfv(1)>0) or
070600041129     c                             (wnfv(2)<>blpnfv and wnfv(2)>0)
070700041129     c                   EXSR      SCRIVIduc
070800091021     c                   else
070900091021     c* Se il foglio è quello di borderizzazione ma la bolla non è più nel
071000091021     c*  fnblp46f, devo trasmettere lo stesso
071100091021     c     kblp46        chain     fnblp46l
071200091021     c                   if        not %found(fnblp46l)
071300091021     c                   EXSR      SCRIVIduc
071400041126     c                   endif
071500091021     c
071600091021     c                   endif
071700041129     c                   endif
071800041126     c
071900950130     C                   ELSE
072000950130     C                   MOVEL     *ZEROS        BLPDUC
072100950130    2C                   END
072200950130     C* DATA PARTENZA PRIMO COLLO
072300950130    2C     WDPC          IFNE      *HIVAL
072400950130     C                   MOVEL     WDPC          BLPDPC
072500950130     C                   ELSE
072600950130     C                   MOVEL     *ZEROS        BLPDPC
072700950130    2C                   END
072800950926     C* DATA SPUNTA ENTRATA
072900950926    2C     WDSE          IFNE      *HIVAL
073000950926     C                   MOVEL     WDSE          BLPDSE
073100950926     C                   ELSE
073200950926     C                   MOVEL     *ZEROS        BLPDSE
073300950926    2C                   END
073400950926     C* DATA ENTRATA TRANSITO
073500950926    2C     WDET          IFNE      *HIVAL
073600950926     C                   MOVEL     WDET          BLPDET
073700950926     C                   ELSE
073800950926     C                   MOVEL     *ZEROS        BLPDET
073900950926    2C                   END
074000950926     C* DATA USCITA TRANSITO
074100950926    2C     WDUT          IFNE      *HIVAL
074200950926     C                   MOVEL     WDUT          BLPDUT
074300950926     C                   ELSE
074400950926     C                   MOVEL     *ZEROS        BLPDUT
074500950926    2C                   END
074600960520     C* DATA ARRIVO
074700960520    2C     WDAM          IFNE      *HIVAL
074800960520     C                   MOVEL     WDAM          BLPDAM
074900960520     C                   ELSE
075000960520     C                   MOVEL     *ZEROS        BLPDAM
075100960520    2C                   END
075200951122     C* VOLUME CML
075300070504     C     agbfvc        IFEQ      'S'
075400951122     C                   Z-ADD     99999         BLPNCR
075500951122     C                   ENDIF
075600990728     C* PESO CML
075700070504     C     agbfpc        IFEQ      'S'
075800990728     C                   Z-ADD     99999         BLPNCP
075900990728     C                   ENDIF
076000950130     C*
076100950130     C                   UPDATE    FNBLP000
076200950130    1C                   ENDIF
076300950130     C*
076400950130     C                   ENDSR
076500950130     C**************************************************************************
076600950130     C* DELETO I RECORD IN FNAGB00F DOPO AVER AGGIORNATO
076700950130     C**************************************************************************
076800950130     C     DELET         BEGSR
076900040309     c                   clear                   werror
077000950130     C*
077100951026     C* NON CANCELLO SE HO TROVATO L'ERRORE
077200040309    1C     *IN99         IFEQ      *OFF
077300070507     c     fnagbnrr      chain(e)  fnagb00f
077400031016     C*
077500031016     C* Gestisco eventuali errori d allocazine record x i quali tramite il
077600031016     C* *PGM d utilità TRUL82R scelgo d inviare 1 MSG alla coda dell'operatore
077700040309    2C                   IF        %error
077800031016     C*
077900031016     C* Valorizzo la DS di procedura
078000031016     C                   CLEAR                   TRUL82DS
078100070507     c                   eval      ul82§rrn = fnagbnrr
078200031016     C                   EVAL      UL82§FIL = 'FNAGB00F'
078300031016     C                   EVAL      UL82§WIN = 'N'
078400031016     C                   EVAL      UL82§F5  = 'S'
078500031016     C*
078600031016     C* Effettuo la chiamata al *PGM d utilità
078700031016     C                   CALL(e)   'TRUL82R'
078800031016     C                   PARM                    TRUL82DS
078900031016     C*
079000040309     c*** Se trovato record allocato vado a fine pgm
079100040309     c                   movel     'E'           werror            1
079200040309     c                   goto      enddel
079300040309    2C                   ENDIF
079400950130     C*
079500070507     c                   if        %found(fnagb00f)
079600070507     C                   DELETE    FNAGBF00
079700070507     c                   endif
079800040309    3C                   ENDIF
079900040309     C     enddel        ENDSR
080000950130     C**************************************************************************
080100950130     C* ELABORO LE BOLLE E AGGIORNO
080200950130     C**************************************************************************
080300950130     C     ELAB          BEGSR
080400110701     c                   eval      $err='0'
080500950130     C*
080600950130     C                   SELECT
080700110701     C* BOLLE ARRIVI flag inoltro
080800110701     C     agbtbo        WHENEQ    'A'
080900110701     C     agbagb        andeq     'ST'
081000110701     C                   EXSR      A_arbfin
081100110701     C* BOLLE PARTENZE flag inoltro
081200110701     C     agbtbo        WHENEQ    'P'
081300110701     C     agbagb        andeq     'ST'
081400110701     C                   EXSR      A_blpfin
081500111007     C* BOLLE SEDE     flag inoltro
081600111007     C     agbtbo        WHENEQ    'S'
081700111007     C     agbagb        andeq     'ST'
081800111007     C                   EXSR      A_tasfin
081900950130     C* BOLLE ARRIVI
082000070504     C     agbtbo        WHENEQ    'A'
082100950130     C                   EXSR      AGGARB
082200950130     C* BOLLE PARTENZE
082300070504     C     agbtbo        WHENEQ    'P'
082400950130     C                   EXSR      AGGBLP
082500950130     C* BOLLE TRANSITO
082600070504     C     agbTBO        WHENEQ    'T'
082700950130     C                   EXSR      AGGBTP
082800950130     C                   ENDSL
082900950130     C*
083000110701     C* DELETO I RECORD senon c'e' stato errore in allocaizone record
083100110701     c                   if        $err='0'
083200950130     C                   EXSR      DELET
083300110701     c                   endif
083400950130     C*
083500950130     C                   ENDSR
083600041104     C**************************************************************************
083700041104     C* ELABORA BOLLE PER AGGIORNARE IL PESO CML
083800041104     C**************************************************************************
083900041104     C     ELAAGP        BEGSR
084000041108     C     1             SETLL     FNAGP00f
084100041108     C                   READ      FNAGP00f
084200041108     C                   DOW       NOT %EOF(FNAGP00f)
084300041104     c
084400041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
084500041108     c*  non penso sarà mai attivabile
084600041108     c                   if        agptbo='P'
084700041104     C     KAgp          CHAIN     FNblp01L                           38
084800041104     c  n38              exsr      APPMOPP
084900070705     c                   if        d39err=*blanks  and o57err=*blanks
085000041108     c                   delete    fnagp000
085100041108     c                   endif
085200041108     c                   endif
085300041108     c
085400041108     C                   READ      FNAGP00f
085500041104     C                   ENDDO
085600111005     c
085700111005     c* Chiudo file  del pgm richiamato
085800111005     c                   clear                   fnlv39ds
085900111005     c                   eval      d39tla='C'
086000111005     C                   MOVEL     fnlv39ds      KPJBU
086100111005     c                   call      'FNLV39R'
086200111005     C                   PARM                    KPJBA
086300111005     c*
086400041104     C                   ENDSR
086500041108     C**************************************************************************
086600041108     C* ELABORA BOLLE PER AGGIORNARE IL volume cML
086700041108     C**************************************************************************
086800041108     C     ELAAGV        BEGSR
086900041108     C     1             SETLL     FNAGV00f
087000041108     C                   READ      FNAGV00f
087100041108     C                   DOW       NOT %EOF(FNAGV00f)
087200041108     c
087300041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
087400041108     c*  non penso sarà mai attivabile
087500041108     c                   if        agvtbo='P'
087600041108     C     KAgv          CHAIN     FNblp01L                           38
087700041108     c  n38              exsr      APPMOVP
087800070705     c                   if        d20err=*blanks  and o57err=*blanks
087900041108     c                   delete    fnagV000
088000041108     c                   endif
088100041108     c                   endif
088200041108     c
088300041108     C                   READ      FNAGV00f
088400041108     C                   ENDDO
088500111005     c
088600111005     c* Chiudo file  del pgm richiamato
088700111005     c                   clear                   fnlv20ds
088800111005     c                   eval      d20tla='C'
088900111005     C                   MOVEL     fnlv20ds      KPJBU
089000111005     c                   call      'FNLV20R'
089100111005     C                   PARM                    KPJBA
089200111005     c
089300041108     C                   ENDSR
089400041104     C*---------------------------------------------------------------*
089500041104     C* ROUTINE DI APPLICAZIONE MODIFICHE PESO A BOLLA PARTENZA
089600041104     C*---------------------------------------------------------------*
089700041104     C     APPMOPP       BEGSR
089800041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
089900041104     C                   Z-ADD     BLPPKF        OLDPKF
090000041104     C                   Z-ADD     BLPPKC        OLDPKC
090100041104     C                   Z-ADD     BLPNCP        OLDNCP
090200041108     C                   Z-ADD     BLPvlF        OLDvlf
090300041108     C                   Z-ADD     BLPvlc        OLDvlc
090400041108     C                   Z-ADD     BLPNCr        OLDNCr
090500070705     c                   movel     blpfvf        OLDfvf
090600041104     C*
090700041108     C                   CLEAR                   fnlv39ds
090800041104     C                   MOVEL     'P'           D39TBO
090900041108     C                   MOVEL     'C'           D39TPP
091000041104     C                   Z-ADD     BLPPKB        D39PLB
091100041104     C                   Z-ADD     BLPPKF        D39PLF
091200041108     C                   Z-ADD     agpPKC        D39PLU
091300041108     C                   Z-ADD     agpNCP        D39NCP
091400041104     C                   Z-ADD     BLPAAS        D39AAS
091500041104     C                   Z-ADD     BLPNRS        D39NRS
091600041104     C                   Z-ADD     BLPLNP        D39LNP
091700041104     C                   Z-ADD     BLPNSP        D39NSP
091800041104     C                   MOVEL     BLPCBO        D39CBO
091900041104     C                   Z-ADD     BLPLNA        D39LNA
092000041104     C                   Z-ADD     BLPNCL        D39NCL
092100041104     C                   Z-ADD     *date         D39DTE
092200041108     C                   MOVEL     fnlv39ds      KPJBU
092300041104     C                   CALL      'FNLV39R'
092400041104     C                   PARM                    KPJBA
092500041108     C                   MOVEL     KPJBU         fnlv39ds
092600041104     C*
092700041104     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
092800041104    3C     D39ERR        IFNE      '1'
092900041104     C                   MOVEL     D39NCP        BLPNCP
093000041104     C                   MOVEL     D39PLU        BLPPKC
093100041104     C                   MOVEL     D39PLF        BLPPKF
093200041104     C* AGGIORNO FILE TASSAZIONE PADRONCINI
093300041108     C                   CLEAR                   fnlv57ds
093400041104     C                   MOVEL     'P'           I57TBO
093500041104     C                   EXSR      AGGTAS
093600070705     c                   if        o57err<>' '
093700070705     C                   MOVEL     OLDNCp        blpNCp
093800070705     C                   MOVEL     OLDpkc        blppkc
093900070705     C                   MOVEL     OLDpkf        blppkf
094000070705     c                   endif
094100070705     c
094200041104     C                   UPDATE    FNBLP000
094300041104E   2C                   ENDIF
094400041104     C*
094500041104     C                   ENDSR
094600041108     C*---------------------------------------------------------------*
094700041108     C* ROUTINE DI APPLICAZIONE MODIFICHE volume A BOLLA PARTENZA
094800041108     C*---------------------------------------------------------------*
094900041108     C     APPMOVP       BEGSR
095000041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
095100041108     C                   Z-ADD     BLPPKF        OLDPKF
095200041108     C                   Z-ADD     BLPPKC        OLDPKC
095300041108     C                   Z-ADD     BLPNCP        OLDNCP
095400041108     C                   Z-ADD     BLPvlF        OLDvlf
095500041108     C                   Z-ADD     BLPvlc        OLDvlc
095600041108     C                   Z-ADD     BLPNCr        OLDNCr
095700070705     c                   movel     blpfvf        OLDfvf
095800041108     C*
095900041108     C                   CLEAR                   fnlv20ds
096000041108     C                   MOVEL     'P'           D20TBO
096100041108     C                   MOVEL     'C'           D20tvl
096200041108     C                   MOVEL     BLPFVB        D20FVB
096300041108     C                   Z-ADD     BLPVLB        D20VLB
096400041108     C                   MOVEL     BLPFVF        D20FVF
096500041108     C                   Z-ADD     BLPVLF        D20VLF
096600041108     C                   Z-ADD     agVVLC        D20VLU
096700041108     C                   Z-ADD     agVNCR        D20NCR
096800041108     C                   Z-ADD     BLPAAS        D20AAS
096900041108     C                   Z-ADD     BLPNRS        D20NRS
097000041108     C                   Z-ADD     BLPLNP        D20LNP
097100041108     C                   Z-ADD     BLPNSP        D20NSP
097200041108     C                   MOVEL     BLPCBO        D20CBO
097300041108     C                   Z-ADD     BLPLNA        D20LNA
097400041108     C                   Z-ADD     BLPNCL        D20NCL
097500041108     C                   Z-ADD     *date         D20DTE
097600041108     C                   MOVEL     fnlv20ds      KPJBU
097700041108     C                   CALL      'FNLV20R'
097800041108     C                   PARM                    KPJBA
097900041108     c                   parm      'N'           pwstor            1
098000041108     C                   MOVEL     KPJBU         fnlv20ds
098100041108     C*
098200041108     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
098300041108    3C     D20ERR        IFNE      '1'
098400041108     C                   MOVEL     D20NCR        BLPNCR
098500041108     C                   MOVEL     D20VLU        BLPVLC
098600041108     C                   MOVEL     D20FVF        BLPFVF
098700041108     C                   MOVEL     D20VLF        BLPVLF
098800041108     C* AGGIORNO FILE TASSAZIONE PADRONCINI
098900041108     C                   CLEAR                   fnlv57ds
099000041108     C                   MOVEL     'P'           I57TBO
099100041108     C                   EXSR      AGGTAS
099200070705     c                   if        o57err<>' '
099300070705     C                   MOVEL     OLDNCR        blpNCR
099400070705     C                   MOVEL     OLDvlc        blpVLC
099500070705     C                   MOVEL     OLDFVF        blpFVF
099600070705     C                   MOVEL     OLDVLF        blpVLF
099700070705     c                   endif
099800070705     c
099900041108     C                   UPDATE    FNBLP000
100000041108E   2C                   ENDIF
100100041108     C*
100200041108     C                   ENDSR
100300041104     C*---------------------------------------------------------------*
100400041104     C* AGGIORNA TASSAZIONE PADRONCINI
100500041104     C*---------------------------------------------------------------*
100600041104     C     AGGTAS        BEGSR
100700041104     C                   MOVEL     blpAAS        I57AAS
100800041104     C                   MOVEL     blpLNP        I57LNP
100900041104     C                   MOVEL     blpNRS        I57NRS
101000041104     C                   MOVEL     blpNSP        I57NSP
101100041104     C                   MOVEL     BLPDRT        I57DRT
101200041104     C                   MOVEL     BLPNRT        I57NRT
101300041104     C                   MOVEL     BLPFPP        I57FPP
101400041104     C                   MOVEL     BLPNCL        I57NCL
101500041104     C                   MOVEL     BLPPDR        I57PDR
101600041108     c* Campi nuovi modificati
101700041104     C                   MOVEL     BLPPKC        I57Pcn
101800041104     C                   MOVEL     BLPncp        I57npn
101900041104     C                   MOVEL     BLPPKf        I57Pfn
102000041108     C                   MOVEL     BLPVLC        I57VCN
102100041108     C                   MOVEL     BLPNCR        I57NCN
102200041108     C                   MOVEL     BLPVLF        I57VFN
102300041108     c* Campi della bolla prima dellamodifica
102400041104     C                   MOVEL     OLDPKC        I57pkc
102500041104     C                   MOVEL     OLDncp        I57ncp
102600041104     C                   MOVEL     OLDPKF        I57PKF
102700041108     C                   MOVEL     OLDVLC        I57VLC
102800041108     C                   MOVEL     OLDNCR        I57NCR
102900041108     C                   MOVEL     OLDVLF        I57VLF
103000041104     C                   MOVEL     'C'           I57TVL
103100041104     C                   CALL      'FNLV57R'
103200041104     C                   PARM                    KPJBA
103300041108     C                   PARM                    fnlv57ds
103400111005     c                   eval      ApriLV57='S'
103500041104     C                   ENDSR
103600041126     C*---------------------------------------------------------------*
103700041126     C* scrivi fidta00f per data partenza ultimo collo
103800041126     C*---------------------------------------------------------------*
103900041129     C     ScriviDUC     BEGSR
104000041129     c* Scrivo DTA solo se cambiata la data
104100041129    1c                   if        wdtaduc<>blpduc
104200041129      * Se è un tipo bolla da trasmettere in sede
104300041129     c                   Clear                   ds3a
104400041129     c                   Z-Add     1             Codut
104500041129     c                   Eval      Cod = '3A'
104600041129     c                   Clear                   Key
104700041129     c                   Movel     blpCbo        Key
104800041129     c     kTab          Chain     Tabel00f
104900041129     c                   If        %Found(Tabel00f)
105000041129     c                   Movel     TblUni        ds3a
105100041129     c                   EndIf
105200041129If  2c                   If        §3aBsd = *Blanks
105300041129      * Scrivo/Aggiorno Fidta
105400041129     c                   Eval      wFidta = '0'
105500041129     c                   Eval      SavPoc = blplnp
105600041129     c                   Eval      SavTrd = 'DUC'
105700041129     c     kFidta        Setll     Fidta01l
105800041129     c                   Do        *Hival
105900041129     c     kFidta        Reade     Fidta01l
106000041129      * Fine file
106100041129     c                   If        %Eof(Fidta01l)
106200041129     c                   Leave
106300041129     c                   EndIf
106400041129      * Già trasmesso
106500041129     c                   If        DtaFtr <> *Blanks
106600041129     c                   Iter
106700041129     c                   EndIf
106800041129      * Aggiorno
106900041129     c                   Eval      DtaDta = blpduc
107000041129     c                   Update    Fidta000
107100041129     c                   Eval      wFidta = '1'
107200041129     c                   EndDo
107300041129      * Scrivo
107400041129     c                   If        wFidta = '0'
107500041129     c                   Clear                   Fidta000
107600041129     c                   Eval      DtaAas = blpAas
107700041129     c                   Eval      DtaLnp = blpLnp
107800041129     c                   Eval      DtaNrs = blpNrs
107900041129     c                   Eval      DtaNsp = blpNsp
108000041129     c                   Eval      DtaTrd = SavTrd
108100041129     c                   Eval      DtaPoc = SavPoc
108200041129     c                   Eval      DtaDta = blpduc
108300041129     c                   Write     Fidta000
108400041129     c                   EndIf
108500041129    2c                   EndIf
108600041129    1c                   EndIf
108700041126     C                   ENDSR
108800110701     C**************************************************************************
108900110701     C* AGGIORNO flag Inoltro bolla arrivi
109000110701     C**************************************************************************
109100110701     C     A_arbfin      BEGSR
109200110701     C*
109300110701     C     KSAV          chain(e)  FNARB01L
109400110701     c                   if        %error
109500110701     c                   eval      $err='1'
109600110701     c                   leavesr
109700110701     c                   endif
109800110701     c
109900110706     c                   if        %found(fnarb01l)  and arbfin<>'I' and
110000110706     c                             arbfin<>'D'
110100110701     c                   eval      arbfin=agbfbs
110200110701     c                   update    fnarb000
110300110706     c                   else
110400110706     c                   unlock    fnarb01l
110500110701     c                   endif
110600110701     c
110700110701     C                   ENDSR
110800110701     C**************************************************************************
110900110701     C* AGGIORNO flag Inoltro bolla Partenza
111000110701     C**************************************************************************
111100110701     C     A_blpfin      BEGSR
111200110701     C*
111300110701     C     KSAV          chain(e)  FNBLP01L
111400110701     c                   if        %error
111500110701     c                   eval      $err='1'
111600110701     c                   leavesr
111700110701     c                   endif
111800110701     c
111900110706     c                   if        %found(fnblp01l) and blpfin<>'I' and
112000110706     c                             blpfin<>'D'
112100110701     c                   eval      BLPfin=agbfbs
112200110701     c                   update    fnblp000
112300110706     c                   else
112400110706     c                   unlock    fnblp01l
112500110701     c                   endif
112600110701     c
112700110701     C                   ENDSR
112800111007     C**************************************************************************
112900111007     C* AGGIORNO flag Inoltro bolla Sede
113000111007     C**************************************************************************
113100111007     C     A_tasfin      BEGSR
113200111007     C*
113300111007     c                   setoff                                       020304
113301130124     c                   clear                   wagg1
113400111007     C     KSAV          chain(e)  titas30c
113500111007     c
113600111011    1c                   if        %error or not %found(titas30c)
113700111007     c                   eval      $err='1'
113800111007     c                   leavesr
113900111011    1c                   endif
114000111007     c
114100111011    1c                   if        %found(titas30c)
114200111011
114300111011    2c                   if        *in02  and tasfic=' '  and tasfin<>'D'
114400111007     c                             and tasfin<>'I'
114500111007     c                   eval      tasfin=agbfbs
114600111007     c                   update    titas000
114601130124     c                   eval      wagg1='S'
114700111011     c
114800111011     c* cerco se c'e' seconda bolla
114900111011     c                   setoff                                       020304
115000111011     C     KSAV          reade(e)  titas30c
115100111011     c
115200111011    3c                   if        %error
115300111011     c                   eval      $err='1'
115400111011     c                   leavesr
115500111011    3c                   endif
115600111011     c                   if        not %eof(titas30c) and
115700111011    3c                             *in02  and tasfic=' '  and tasfin<>'D'
115800111011     c                             and tasfin<>'I'
115900111011     c                   eval      tasfin=agbfbs
116000111011     c                   update    titas000
116100111011    3c                   endif
116200111011    c
116300111011    2c                   endif
116301130124     c* Già contabilizzata - I bolla
116302130124     c* se inoltro non aggiornato perchè bolla già contabilizzata
116303130124     c* bisogna scrivere file per recupero variazioni post-fatturazione
116304130305     c* Non scrivo mai tivrb per la seconda bolla in quanto il flag
116305130305     c* inoltro non influisce sulla tassazione della seconda bolla
116306130305    2c                   if        wagg1=*blanks and tasfin<>'D' and
116307130305     c                                               tasfin<>'I'
116308130305     c                   exsr      sr_tivrb
116309130305    2c                   endif
116400111011    1c                   endif
116500111007
116600111007     c                   unlock    titas30c
116700111007     c
116800111007     C                   ENDSR
116801130201     C**************************************************************************
116802130201     C* Richiamo pgm gestione file recupero variazioni post-fatturaz. (TIVRB)
116803130201     C**************************************************************************
116804130201     C     sr_tivrb      BEGSR
116805130201     c                   clear                   fnlvv1ds
116806130201     c                   eval      iv1cvb='ZT'
116807130201     c                   eval      iv1pgm=nompgm
116808130201     c                   eval      iv1aas=tasaas
116809130201     c                   eval      iv1lnp=taslnp
116810130201     c                   eval      iv1nrs=tasnrs
116811130201     c                   eval      iv1nsp=tasnsp
116812130201     c                   eval      iv1tbl=tastbl
116813130201     c                   clear                   parvar
116814130201     c                   movel     agbfbs        parvar
116815130204     c                   movel     fnlvv1ds      kpjbu
116816130206     c                   call      'FNLVV1R'
116817130204     c                   parm                    kpjba
116818130201     c                   parm                    parvar
116819130201     c                   endsr
116900000000     C**************************************************************************
117000000000     C* R O U T I N E    I N I Z I A L E
117100000000     C**************************************************************************
117200000000     C     *INZSR        BEGSR
117300950130     C*
117400000000     C     KART          KLIST
117500000000     C                   KFLD                    ARTAAS
117600000000     C                   KFLD                    ARTLNP
117700000000     C                   KFLD                    ARTNRS
117800000000     C                   KFLD                    ARTNSP
117900950130     C     KBTT          KLIST
118000950130     C                   KFLD                    BTTAAS
118100950130     C                   KFLD                    BTTLNP
118200950130     C                   KFLD                    BTTNRS
118300950130     C                   KFLD                    BTTNSP
118400031127     C     KBTP          KLIST
118500031127     C                   KFLD                    BTPflp
118600031127     C                   KFLD                    BTpAAS
118700031127     C                   KFLD                    BTpLNP
118800031127     C                   KFLD                    BTpNRS
118900031127     C                   KFLD                    BTpNSP
119000950130     C     KBLT          KLIST
119100950130     C                   KFLD                    BLTAAS
119200950130     C                   KFLD                    BLTLNP
119300950130     C                   KFLD                    BLTNRS
119400950130     C                   KFLD                    BLTNSP
119500041104     C     KAGp          KLIST
119600041104     C                   KFLD                    AGpAAS
119700041104     C                   KFLD                    AGpLNP
119800041104     C                   KFLD                    AGpNRS
119900041104     C                   KFLD                    AGpNSP
120000041108     C     KAGV          KLIST
120100041108     C                   KFLD                    AGvAAS
120200041108     C                   KFLD                    AGvLNP
120300041108     C                   KFLD                    AGvNRS
120400041108     C                   KFLD                    AGvNSP
120500000000     C     KSAV          KLIST
120600070504     C                   KFLD                    agbAAS
120700070504     C                   KFLD                    agbLNP
120800070504     C                   KFLD                    agbNRS
120900070504     C                   KFLD                    agbNSP
121000030108
121100030108     c     kFidta        Klist
121200070504     c                   Kfld                    agbAas
121300070504     c                   Kfld                    agbLnp
121400070504     c                   Kfld                    agbNrs
121500070504     c                   Kfld                    agbNsp
121600030108     c                   Kfld                    SavTrd
121700030108     c                   Kfld                    SavPoc
121800091021     C     Kblp46        KLIST
121900091021     C                   KFLD                    BlPtfp
122000091021     C                   KFLD                    BlpAAS
122100091021     C                   KFLD                    BlpLNP
122200091021     C                   KFLD                    BlpNRS
122300091021     C                   KFLD                    BlpNSP
122400030320
122500030320     c     kTab          Klist
122600030320     c                   Kfld                    Codut
122700030320     c                   Kfld                    Cod
122800030320     c                   Kfld                    Key
122900000000     C*
123000950926     C     *LIKE         DEFINE    BLPDSE        WDSE
123100950926     C     *LIKE         DEFINE    ARBDET        WDET
123200950130     C     *LIKE         DEFINE    ARBDUT        WDUT
123300000000     C     *LIKE         DEFINE    ARTDAM        WDAM
123400950328     C     *LIKE         DEFINE    ARTDAM        WDAU
123500950130     C     *LIKE         DEFINE    ARBDPC        WDPC
123600950130     C     *LIKE         DEFINE    ARBDUC        WDUC
123700950130     C     *LIKE         DEFINE    ARBFAN        WFAN
123800000000     C*
123900000000     C                   ENDSR
