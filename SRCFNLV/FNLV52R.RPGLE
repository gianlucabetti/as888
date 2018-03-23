000100041104     H DECEDIT('0,') DATEDIT(*ymd.)
000200950130     F* FNLV52R *-----------------------------------------------------*
000300950130     F*       AGGIORNAMENTI VARI SULLE BOLLE
000400950130     F*---------------------------------------------------------------*
000500091021     FFNblp46l  IF   E           K DISK    rename(fnblp000:fnablp046)
000600091021     FFNBTT11L  IF   E           K DISK
000700031127     FFNBTP01L  UF   E           K DISK
000800950130     F*
000900950130     FFNART01L  IF   E           K DISK
000901180122     FFiART01L  IF   E           K DISK
000902180122     FFiARs01L  IF   E           K DISK
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
002201180129     fTitah33c  uf a e           k Disk    UsrOpn extfile(tah33)
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
006200160726     d wopenlsu2       s              1
006201180122     d w_tas           s              1
006300030108
006400070507     D fnagbinf        ds
006500070507     D  fnagbnrr             397    400i 0
006600960229     D                 DS
006700960229     D  ARBAAS                 1      4  0
006800960229     D  ARBMGS                 5      8  0
006900960229     D  ARBDSP                 1      8  0
007000970414     D WLBDAT          DS                  INZ
007100970414     D  G02DAT                 1      8  0
007200970414     D  G02INV                 9     16  0
007300970414     D  G02ERR                17     17
007400970414     D  G02TGI                18     22  0
007500970414     D WGIDAT          DS                  INZ
007600970414     D  GIODAT                 1      8  0
007700970414     D  GIOINV                 9     16  0
007800970414     D  GIOTGI                17     21  0
007900150605     d                 ds
008000150605     d bsplin
008100150605     d sbsp                           3  0 dim(30) overlay(bsplin)
008200111006     d
008300950130     D* PARAMETRI DI PASSAGGIO
008400950130     D PARAM           DS
008500950130     D  PARTBO                 1      1
008600160726
008700160726     D  FNLSU2R3DS     DS
008800160726     D  sa2tla                        1
008900160726     D  sa2trc                        1
009000160726     D  sa2dts                        8  0
009100160726     D  sa2lna                        3  0
009200160726     D  sa2ncl                        5  0
009300160726     D  sa2tfp                        3  0
009400160726
009500111006     d
009600950130     D KPJBA         E DS
009700150605     D* REPERISCE DATI FOGLI
009800150605     D DSLV53        E DS                  EXTNAME(FNLV53DS)
009900031016      * DS Esterna x gestione allocazione ed invio messaggi
010000031016     D TRUL82DS      E DS
010100041108     D fnlv39ds      E DS
010200041108     D fnlv20ds      E DS
010300041108     D fnlv57ds      E DS
010400130204     D fnlvv1ds      E DS
010500150605     D fnls65ds      E DS
010501170908     D fnlsi4ds1     E DS
010600150625     D trul06ds      E DS
010700150625     D  L06                    1     90  0 dim(30)
010800030320
010900030320     d ds3a          e ds
011000150605     d og143         e ds
011100160722     d DAGBFLO       e ds
011101180122     d Dtasnas       e ds
011200110701     d
011300111007     d TAS30           s             21    inz('GAITRAGRPS/TITAS30C')
011301180129     d TAH33           s             21    inz('GAITRAGRPS/TITAH33C')
011400130124     D                SDS
011500130124     D  NOMPGM                 1     10
011600111007     d
011700111007     ITITAS000      02
011800111007     ITITAS010      03
011900111007     ITITASP00      04
011901180123     ifiart000
011902180123     i              artnfv                      artnfvi
012000950130     I*----------------------------------------------------------------
012100950130     C     *ENTRY        PLIST
012200950130     C                   PARM                    KPJBA
012300040514     C                   PARM                    chiudi            1
012400111006     c
012500040514     C                   SETOFF                                       30
012600111005     c                   clear                   ApriLV57          1
012700111007     c
012800111007     c* Se non aperto apro il file
012900111007     c                   if        not %open(titas30c)
013000111007     C                   OPEN(e)   titas30c
013100111007     c
013200111007     c                   if        not %open(titas30c)
013300111007     c                   eval      %subst(tas30:7:4)='GRU '
013400111007     C                   OPEN      titas30c
013401180131     c                   eval      %subst(tah33:7:4)='GRU '
013402180131     c                   OPEN      titah33c
013403180131     c                   else
013405180131     C                   OPEN      titah33c
013500111007     c                   endif
013600111007     c                   endif
013601180122
013700950130     C*
013800950130     C* LETTURA DEL FILE
013900970117    1C     PARTBO        IFEQ      ' '
014000070504     C     *LOVAL        SETLL     FNAGB02L
014100950130     C                   ELSE
014200070504     C     PARTBO        SETLL     FNAGB02L
014300970117    1C                   ENDIF
014400040315     c
014500040315    1c                   dow       not *in30
014600950130     C*
014700040315    2C     PARTBO        IFEQ      ' '
014800070507     C                   READ      FNAGB02L                               30
014900950130     C                   ELSE
015000070507     C     PARTBO        READE     FNAGB02L                               30
015100040315    2C                   ENDIF
015200950130     C*
015300111006     c* Escludo tipo bolla "S"
015400111010    2C                   if        not *in30
015500950130     C*
015600950130     C                   EXSR      ELAB
015700040309     c
015800070507    3c                   if        werror='E'
015900040309     c                   goto      fine
016000070507    3c                   endif
016100070507    2C                   ENDIF
016200950130     C*
016300040315    1C                   ENDDO
016400040315     c
016500041108     C
016600041108     c     fine          tag
016700040309     c
016800041104     C* ELABORA LE BOLLE PER IL PESO
016900041104     C                   EXSR      ELAAGP
017000041108     C* ELABORA LE BOLLE PER IL volume
017100041108     C                   EXSR      ELAAGV
017200111005
017300111005     c                   if        ApriLV57='S'
017400111005     c                   clear                   fnlv57ds
017500111005     c                   eval      i57tla='C'
017600111005     c                   call      'FNLV57R'
017700111005     C                   PARM                    KPJBA
017800111005     C                   PARM                    fnlv57ds
017900111005     c                   endif
018000130201     C                   MOVEL     'C'           iv1tla
018100130201     c                   movel     fnlvv1ds      kpjbu
018200130206     C                   CALL      'FNLVV1R'
018300130201     C                   PARM                    kpjba
018400150605
018500150605     C                   CLEAR                   fnls65ds
018600150605     C                   MOVEL     'C'           i65tla
018700150605     c                   movel     '1'           I65STRCMT
018800150605     c                   movel     fnls65ds      kpjbu
018900150605     C                   CALL      'FNLS65R'
019000150605     C                   PARM                    kpjba
019001170908
019002170908     C                   CLEAR                   fnlsi4ds1
019003170908     C                   MOVEL     'C'           ii4tla
019004170908     c                   movel     '1'           Ii4STRCMT
019005170908     c                   movel     fnlsi4ds1     kpjbu
019006170908     C                   CALL      'FNLSI4R1'
019007170908     C                   PARM                    kpjba
019100150625
019200150625     c                   clear                   trul06ds
019300150625     c                   movel     'C'           d06tla
019400150625     c                   movel     trul06ds      kpjbu
019500150625     C                   CALL      'TRUL06R'
019600150625     C                   PARM                    kpjba
019700160726
019800160726     c                   if        wopenlsu2<>' '
019900160726     c                   clear                   fnlsu2r3ds
020000160726     c                   eval      sa2tla='C'
020100160726     c                   call      'FNLSU2R3'
020200160726     c                   parm                    fnlsu2r3ds
020300160726     c                   endif
020400130201     c
020500040514     C*
020600040514     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
020700040514     C                   SHTDN                                        98
020800040514     c
020900040514     c                   if        *in98
021000040514     c                   eval      chiudi='S'
021100111006     C****               SETON                                        LR
021200040514     c                   else
021300111005
021400111006     C****               SETON                                        RT
021500040514     c                   endif
021600111006     c
021700111006     C                   SETON                                        LR
021800950130     C*
021801180122     C**************************************************************************
021802180122     C* AGGIORNA/delete incompatibile su bolle di sede
021803180122     C**************************************************************************
021804180122     C     agg_inc       BEGSR
021805180123     c* prima di tutto verifico di avere titas
021816180123     c* perchè se la bolla non è ancora in sede
021817180123     c* lascio il record in agb in attesa dell'arrivo della bolla su titas
021818180131     c                   setoff                                       020304
021819180131           chain(N) (agbaas:agblnp:AGBNRS:AGBNSP) titas30c ;
021820180131           if not %found(titas30c)     ;
021821180131              $err='1';
021822180131              leavesr;
021823180131           endif;
021824180131
021829180122        clear  w_tas;
021830180122        setll (agbaas:agblnp:AGBNRS:AGBNSP) fiART01L ;
021831180122        READE (agbaas:agblnp:AGBNRS:AGBNSP) FIART01L ;
021832180122        dow not %eof(fiart01l);
021833180122           chain (artfls:artlna:artnrs:artnsc:'J') fiars01l;
021834180122           if %found(fiars01l);
021835180129             SETLL (AGBAAS:agblnp:agbnrs:agbnsp:'J':arsnsc) titah33c;
021836180129              if not %equal(titah33c);
021837180122                 clear titah000 ;
021838180122                 TAHAAS=agbaas   ;
021839180122                 TAHLNP=agblnp   ;
021840180122                 TAHNRS=agbnrs   ;
021841180122                 TAHNSP=agbnsp   ;
021842180122                 TAHTRC=ARSTRC   ;
021843180122                 TAHNOT=ARSNOT   ;
021844180122                 TAHnsc=ARSnsc   ;
021845180131                 if *in04;
021846180131                     write titahp00;
021847180131                 else;
021848180131                     write titah000;
021849180131                 endif;
021850180129              endif     ;
021851180129                 w_tas='S' ;
021852180122           else;
021853180122       //    se non c'è lo cancello anche su titah
021854180122              if agbagb='JD'  ;
021855180129       //        exec sql
021856180129       //        delete from  titah00f
021857180129       //        where tahaas = :agbaas and tahlnp = :agblnp and
021858180129       //              tahnrs = :agbnrs and tahnsp = :agbnsp and
021859180129       //              tahtrc = 'J'     and tahnsc=  :artnsc      ;
021860180129                 delete (agbaas:agblnp:agbnrs:agbnsp:'J':artnsc) titah33c  ;
021861180122              endif;
021862180122           endif;
021863180122           READE (agbaas:agblnp:AGBNRS:AGBNSP) FIART01L ;
021864180122        enddo;
021865180123
021866180123     c                   setoff                                       020304
021867180123           chain(e) (agbaas:agblnp:AGBNRS:AGBNSP) titas30c ;
021868180123    1c                   if        %error or not %found(titas30c)
021869180123     c                   eval      $err='1'
021870180123     c                   leavesr
021871180123    1c                   endif
021872180122           if %found(titas30c);
021873180122             dtasnas=tasnas;
021874180122        //metto o tolgo il flag su Titas
021875180122             if w_tas='S'       ;
021876180122                §floinc='S'   ;
021877180122             else;
021878180122                §floinc=' '   ;
021879180122             endif;
021880180122             tasnas=dtasnas;
021881180122     c   02              update    titas000
021882180123     c   03              update    titas010
021883180122     c   04              update    titasp00
021884180122           endif;
021885180122     c                   endsr
021900950130     C**************************************************************************
022000950130     C* AGGIORNO BOLLE  ARRIVI
022100950130     C**************************************************************************
022200950130     C     AGGARB        BEGSR
022300950130     C                   MOVEL     *HIVAL        WDPC
022400950130     C                   MOVEL     *HIVAL        WDET
022500950130     C                   MOVEL     *HIVAL        WDAM
022600950130     C                   Z-ADD     *ZERO         WDUT
022700950130     C                   Z-ADD     *ZERO         WDUC
022800950328     C                   Z-ADD     *ZERO         WDAU
022900950122     C                   CLEAR                   WFAN
023000951026     C                   SETOFF                                       99
023100000000     C*
023200000000     C     KSAV          SETLL     FNART01L
023300000000     C     KSAV          READE     FNART01L                               34
023400000000     C*
023500950130    1C     *IN34         DOWEQ     '0'
023600950130     C*
023700950130     C* CAUSALE PER AGGIORNAMENTO DATE
023800070504    2C     agbagb        IFEQ      'DT'
023900000000     C*
024000950130     C* DATA ENTRATA AL TRANSITO
024100950130    3C     ARTDET        IFGT      0
024200950130     C     ARTDET        ANDLT     WDET
024300950130     C                   MOVEL     ARTDET        WDET
024400950130    3C                   END
024500000000     C*
024600000000     C* DATA USCITA DAL TRANSITO
024700950130    3C     ARTDUT        IFGT      0
024800950130     C     ARTDUT        ANDGT     WDUT
024900950130     C                   MOVEL     ARTDUT        WDUT
025000950130    3C                   END
025100950130     C*
025200950130     C* DATA PARTENZA 1 COLLO
025300950130    3C     ARTDFV        IFGT      0
025400950130    4C     ARTDFV        IFLT      WDPC
025500950130     C                   MOVEL     ARTDFV        WDPC
025600950130    4C                   END
025700950130     C* DATA PARTENZA ULTIMO COLLO
025800950130    4C     ARTDFV        IFGT      WDUC
025900950130     C                   MOVEL     ARTDFV        WDUC
026000950130    4C                   END
026100950130     C*
026200950130   X3C                   ELSE
026300950130     C                   Z-ADD     *HIVAL        WDUC
026400950130    3C                   END
026500950328     C* CODICE  ANOMALIA
026600950328    3C     ARTCAN        IFNE      *BLANKS
026700950328     C                   MOVEL     'S'           WFAN
026800950328    3C                   ENDIF
026900950328    2C                   END
027000950130     C*
027100950328     C* CAUSALE PER AGGIORNAMENTO DATE
027200070504    2C     agbagb        IFEQ      'DT'
027300950328     C* CAUSALE DI AGGIORNAMENTO DA ABBINAMENTO
027400070504    2C     agbagb        OREQ      'AB'
027500950328     C*
027600950328    3C     ARTDAM        IFGT      0
027700950328    4C     ARTDAM        IFLT      WDAM
027800950328     C                   MOVEL     ARTDAM        WDAM
027900950328    4C                   END
028000950918     C* DATA ARRIVO  ULTIMO COLLO LEGGENDO DA ARTDAM
028100950328    4C     ARTDAM        IFGT      WDAU
028200950328     C                   MOVEL     ARTDAM        WDAU
028300950328    4C                   END
028400950328     C*
028500950328   X3C                   ELSE
028600950328     C                   Z-ADD     *HIVAL        WDAU
028700950328    3C                   END
028800950130    2C                   ENDIF
028900000000     C*
029000000000     C     KSAV          READE     FNART01L                               34
029100950130    1C                   ENDDO
029200000000     C*
029300000000     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
029400000000     C*   TRANSITO E FILIALE TRANSITO
029500951026     C     KSAV          CHAIN     FNARB01L                           3099
029600000000     C*
029700951026    1C     *IN30         IFEQ      *OFF
029800951026     C     *IN99         ANDEQ     *OFF
029900040401     c                   z-add     arbdam        wdtadam
030000950328     C* CAUSALE PER AGGIORNAMENTO DATE
030100070504   1AC     agbagb        IFEQ      'DT'
030200950130     C* DATA ENTRATA AL TRANSITO
030300950130    2C     WDET          IFNE      *HIVAL
030400950130     C                   MOVEL     WDET          ARBDET
030500000000     C                   ELSE
030600000000     C                   MOVEL     *ZEROS        ARBDET
030700950130    2C                   END
030800000000     C* DATA USCITA DAL TRANSITO
030900950130     C                   Z-ADD     WDUT          ARBDUT
031000950130     C* DATA PARTENZA ULTIMO COLLO
031100950130    2C     WDUC          IFNE      *HIVAL
031200950130     C                   MOVEL     WDUC          ARBDUC
031300950130     C                   ELSE
031400950130     C                   MOVEL     *ZEROS        ARBDUC
031500950130    2C                   END
031600950130     C* DATA PARTENZA PRIMO COLLO
031700950130    2C     WDPC          IFNE      *HIVAL
031800950130     C                   MOVEL     WDPC          ARBDPC
031900950130     C                   ELSE
032000950130     C                   MOVEL     *ZEROS        ARBDPC
032100950130    2C                   END
032200000000     C*
032300000000     C* SE TROVATO COD.ANOMALIA SUI COLLI IMPOSTO IL FLAG SULLA BOLLA
032400950130    2C     WFAN          IFEQ      'S'
032500000000     C                   MOVEL     'S'           ARBFAN
032600950130    2C                   ENDIF
032700950328   1AC                   ENDIF
032800950328     C* DATA ARRIVO
032900950328    2C     WDAM          IFNE      *HIVAL
033000950328     C                   MOVEL     WDAM          ARBDAM
033100950328     C                   ELSE
033200950328     C                   MOVEL     *ZEROS        ARBDAM
033300950328    2C                   END
033400950918     C*
033500950918     C* SE BOLLA LOCALE E CI SONO TUTTE LE DATE DI ARRIVO
033600950918     C*  AGGIORNO CON L'ULTIMA DATA, LA DATA ARRIVO ULTIMO COLLO
033700070504   1AC     agbagb        IFEQ      'DT'
033800040514     C
033900040226     c
034000040226    2C     arbtfp        IFEQ      arbtfa
034100040226     c
034200970414    3C     WDAU          IFNE      *HIVAL
034300970414     C* SOMMO 1 ALLA DATA BORDERO' E PRENDO LA DATA PIU' ALTA
034400970414     C*  TRA I COLLI  E LA BOLLA
034500970414     C                   Z-ADD     ARBDBR        G02INV
034600970414     C                   MOVEL     '3'           G02ERR
034700970414     C                   CALL      'XSRDA8'
034800970414     C                   PARM                    WLBDAT
034900970414     C     G02TGI        ADD       1             GIOTGI
035000970414     C*
035100970414     C                   CALL      'XSRGI8'
035200970414     C                   PARM                    WGIDAT
035300970414     C*
035400970414     C                   Z-ADD     GIOINV        DATAP             8 0
035500970414     C**
035600970414    4C     DATAP         IFGT      WDAU
035700970414     C                   Z-ADD     DATAP         ARBDTI
035800970414     C                   ELSE
035900970414     C                   Z-ADD     WDAU          ARBDTI
036000970414    4C                   ENDIF
036100960229     C* LA DATA ARRIVO ULTIMO COLLO NON PUO' ESSERE INFERIORE
036200960229     C*  ALLA DATA SPEDIZIONE
036300970414    4C     ARBDTI        IFLT      ARBDSP
036400960229     C                   Z-ADD     ARBDSP        ARBDTI
036500970414    4C                   ENDIF
036600970924     C     ARBFBS        IFEQ      ' '
036700950922     C                   MOVEL     'S'           ARBFBS
036800970924     C                   ELSE
036900970924     C     ARBFBS        IFEQ      'V'
037000040505     C     ARBFBS        oreq      'P'
037100970924     C                   MOVEL     'E'           ARBFBS
037200970924     C                   ENDIF
037300970924     C                   ENDIF
037400970924     C**
037500970414   X3C                   ELSE
037600970414     C*
037700970414     C* NON C'E' LA DATA DI ARRIVO SU TUTTI I COLLI
037800970414     C                   CLEAR                   ARBDTI
037900970414    3C                   ENDIF
038000970414    2C                   ENDIF
038100950918   1AC                   ENDIF
038200950918     C*
038300950328     C* SE C'E' DATA ULTIMO COLLO E < DI QUELLA GIA' CALCOLATA, AZZERO
038400950328     C* PER FARLA RICALCOLARE
038500070504   1AC     agbagb        IFEQ      'AB'
038600950328     C                   CLEAR                   ARBDTI
038700950328     C                   CLEAR                   ARBHTI
038800950328     C                   ENDIF
038900950922     C* TOCCO BOLLA
039000070504     C     agbfbs        IFEQ      'S'
039100970924     C     ARBFBS        IFEQ      ' '
039200970924     C                   MOVEL     'S'           ARBFBS
039300970924     C                   ELSE
039400970924     C     ARBFBS        IFEQ      'V'
039500040505     C     ARBFBS        oreq      'P'
039600970924     C                   MOVEL     'E'           ARBFBS
039700970924     C                   ENDIF
039800970924     C                   ENDIF
039900950922     C                   ENDIF
040000950922     C* VOLUME CML
040100070504  1  C     agbfvc        IFEQ      'S'
040200070504     C     agbfpc        OREQ      'S'
040300990728  2  C     ARBFBS        IFEQ      ' '
040400070504  3  C     agbfvc        IFEQ      'S'
040500990728     C                   MOVEL     'V'           ARBFBS
040600990728 E3  C                   ENDIF
040700070504  3  C     agbfpc        IFEQ      'S'
040800990728  4  C     ARBFBS        IFEQ      ' '
040900990728     C                   MOVEL     'P'           ARBFBS
041000990728 X4  C                   ELSE
041100990728     C                   MOVEL     'E'           ARBFBS
041200990728 E4  C                   END
041300990728 E3  C                   END
041400990728 X2  C                   ELSE
041500990728  3  C     ARBFBS        IFEQ      'S'
041600970924     C                   MOVEL     'E'           ARBFBS
041700990728  E3 C                   ENDIF
041800990728  E2 C                   ENDIF
041900990728  E1 C                   ENDIF
042000950922     C*
042100000000     C                   UPDATE    FNARB000
042200040401     c
042300040401     c* Scrivo DTA solo se cambiata la data
042400040401     c                   if        wdtadam<>arbdam
042500030320      * Se è un tipo bolla da trasmettere in sede
042600030320     c                   Clear                   ds3a
042700111006     c                   Z-Add     1             Codut             1 0
042800030320     c                   Eval      Cod = '3A'
042900030320     c                   Clear                   Key
043000030320     c                   Movel     ArbCbo        Key
043100030320     c     kTab          Chain     Tabel00f
043200030320     c                   If        %Found(Tabel00f)
043300030320     c                   Movel     TblUni        ds3a
043400030320     c                   EndIf
043500030320If  2c                   If        §3aBsd = *Blanks
043600030206      * Scrivo/Aggiorno Fidta
043700030109     c                   Eval      wFidta = '0'
043800030108     c                   Eval      SavPoc = ArbLna
043900030108     c                   Eval      SavTrd = 'DAM'
044000030109     c     kFidta        Setll     Fidta01l
044100030109     c                   Do        *Hival
044200030109     c     kFidta        Reade     Fidta01l
044300030109      * Fine file
044400030109     c                   If        %Eof(Fidta01l)
044500030109     c                   Leave
044600030109     c                   EndIf
044700030109      * Già trasmesso
044800030109     c                   If        DtaFtr <> *Blanks
044900030109     c                   Iter
045000030109     c                   EndIf
045100030109      * Aggiorno
045200030109     c                   Eval      DtaDta = ArbDam
045300030108     c                   Update    Fidta000
045400030109     c                   Eval      wFidta = '1'
045500030109     c                   EndDo
045600030109      * Scrivo
045700030109     c                   If        wFidta = '0'
045800030108     c                   Clear                   Fidta000
045900030108     c                   Eval      DtaAas = ArbAas
046000030108     c                   Eval      DtaLnp = ArbLnp
046100030108     c                   Eval      DtaNrs = ArbNrs
046200030108     c                   Eval      DtaNsp = ArbNsp
046300030108     c                   Eval      DtaTrd = SavTrd
046400030108     c                   Eval      DtaPoc = SavPoc
046500030109     c                   Eval      DtaDta = ArbDam
046600030108     c                   Write     Fidta000
046700030108     c                   EndIf
046800030320    2c                   EndIf
046900040401    2c                   EndIf
047000030108
047100950130    1C                   ENDIF
047200000000     C*
047300000000     C                   ENDSR
047400950130     C**************************************************************************
047500950130     C* AGGIORNO BOLLE  TRANSITO
047600950130     C**************************************************************************
047700950130     C     AGGBTP        BEGSR
047800951026     C                   SETOFF                                       99
047900031127     c* Per il momento leggiamo se richieso l'aggiornamento di 1 bolla
048000031127     c*  transito, tutte le bolle transito per i vari p.o. perchè non
048100031127     c*  sono ancora in grado di sapere per quale p.o. transito è stato
048200031127     c*  richiesto l'aggiornamento
048300950130     C*
048400031127     C     KSAV          SETLL     FNBTP01L
048500031127     C     KSAV          READE     FNBTP01L                             9934
048600031127     c
048700031127    0c                   dow       not *in34 and not *in99
048800031127     C                   MOVEL     *HIVAL        WDPC
048900031127     C                   MOVEL     *HIVAL        WDET
049000031127     C                   MOVEL     *HIVAL        WDAM
049100031127     C                   Z-ADD     *ZERO         WDUT
049200031127     C                   Z-ADD     *ZERO         WDUC
049300040401     c                   eval      wdtadet=btpdet
049400040401     c                   eval      wdtadut=btpdut
049500031127     C                   CLEAR                   WFAN
049600031128     C                   CLEAR                   Wagv              1
049700031128     C                   CLEAR                   Wagp              1
049800031127     c
049900031127     C     Kbtp          SETLL     FNBTT11L
050000031127     C     Kbtp          READE     FNBTT11L                               34
050100950130     C*
050200950130    1C     *IN34         DOWEQ     '0'
050300950130     C*
050400950130     C* CAUSALE PER AGGIORNAMENTO DATE
050500070504    2C     agbagb        IFEQ      'DT'
050600031128     c* aggiorno il campo del volume e del peso sulla testata, se è presente
050700031128     c*  almeno un volume o peso. Infatti col fatto che ci possono essere +
050800031128     c*  bolle, magari una ce lo ha e le altre no!!
050900031128     c                   if        bttvuc>0
051000031128     c                   eval      wagv='S'
051100031128     c                   endif
051200031128     c                   if        bttpuc>0
051300031128     c                   eval      wagp='S'
051400031128     c                   endif
051500950130     C*
051600950130     C* DATA ENTRATA AL TRANSITO
051700950130    3C     BTTDET        IFGT      0
051800950130     C     BTTDET        ANDLT     WDET
051900950130     C                   MOVEL     BTTDET        WDET
052000950130    3C                   END
052100950130     C*
052200950130     C* DATA USCITA DAL TRANSITO
052300950130    3C     BTTDUT        IFGT      0
052400950130     C     BTTDUT        ANDGT     WDUT
052500950130     C                   MOVEL     BTTDUT        WDUT
052600950130    3C                   END
052700950130     C*
052800950130     C* DATA PARTENZA 1 COLLO
052900950130    3C     BTTDFV        IFGT      0
053000950130    4C     BTTDFV        IFLT      WDPC
053100950130     C                   MOVEL     BTTDFV        WDPC
053200950130    4C                   END
053300950130     C* DATA PARTENZA ULTIMO COLLO
053400950130    4C     BTTDFV        IFGT      WDUC
053500950130     C                   MOVEL     BTTDFV        WDUC
053600950130    4C                   END
053700950130     C*
053800950130   X3C                   ELSE
053900950130     C                   Z-ADD     *HIVAL        WDUC
054000950130    3C                   END
054100950130     C*
054200950130     C* DATA ARRIVO
054300950130    3C     BTTDAM        IFGT      0
054400950130     C     BTTDAM        ANDLT     WDAM
054500950130     C                   MOVEL     BTTDAM        WDAM
054600950130    3C                   END
054700950130    2C                   ENDIF
054800950130     C*
054900031127     C     Kbtp          READE     FNBTT11L                               34
055000950130    1C                   ENDDO
055100950130     C*
055200950130     C* DATA ENTRATA AL TRANSITO
055300950130    2C     WDET          IFNE      *HIVAL
055400950130     C                   MOVEL     WDET          BTPDET
055500950130     C                   ELSE
055600950130     C                   MOVEL     *ZEROS        BTPDET
055700950130    2C                   END
055800950130     C* DATA USCITA DAL TRANSITO
055900950130     C                   Z-ADD     WDUT          BTPDUT
056000160726     c* Verifico rielaborazione statistiche
056100160726     c                   if        §agbdsf='S'  and wdut>0
056200160726     c                   clear                   fnlsu2r3ds
056300160726     c                   eval      sa2trc='E'
056400160726     c                   eval      sa2dts=wdut
056500160726     c                   eval      sa2ncl=btpncl
056600160726     c                   eval      sa2tfp=btpflp
056700160726     c                   call      'FNLSU2R3'
056800160726     c                   parm                    fnlsu2r3ds
056900160726     c                   eval      wopenlsu2='S'
057000160726
057100160726     c                   endif
057200950130     C* DATA ARRIVO
057300950130    2C     WDAM          IFNE      *HIVAL
057400950130     C                   MOVEL     WDAM          BTPDAM
057500950130     C                   ELSE
057600950130     C                   MOVEL     *ZEROS        BTPDAM
057700950130    2C                   END
057800950130     C* DATA PARTENZA ULTIMO COLLO
057900950130    2C     WDUC          IFNE      *HIVAL
058000950130     C                   MOVEL     WDUC          BTPDUC
058100950130     C                   ELSE
058200950130     C                   MOVEL     *ZEROS        BTPDUC
058300950130    2C                   END
058400950130     C* DATA PARTENZA PRIMO COLLO
058500950130    2C     WDPC          IFNE      *HIVAL
058600950130     C                   MOVEL     WDPC          BTPDPC
058700950130     C                   ELSE
058800950130     C                   MOVEL     *ZEROS        BTPDPC
058900950130    2C                   END
059000950922     C* VOLUME CML
059100070504     C     agbfvc        IFEQ      'S'
059200031128     c     WAGV          andeq     'S'
059300950922     C                   Z-ADD     99999         BTPNCR
059400950922     C                   ENDIF
059500990728     C* PESO CML
059600070504     C     agbfpc        IFEQ      'S'
059700031128     c     WAGP          andeq     'S'
059800990728     C                   Z-ADD     99999         BTPNCP
059900990728     C                   ENDIF
060000950130     C*
060100950130     C                   UPDATE    FNBTP000
060200040401     c
060300030320      * Se è un tipo bolla da trasmettere in sede
060400030320     c                   Clear                   ds3a
060500030320     c                   Z-Add     1             Codut
060600030320     c                   Eval      Cod = '3A'
060700030320     c                   Clear                   Key
060800030320     c                   Movel     BtpCbo        Key
060900030320     c     kTab          Chain     Tabel00f
061000030320     c                   If        %Found(Tabel00f)
061100030320     c                   Movel     TblUni        ds3a
061200030320     c                   EndIf
061300030320If  2c                   If        §3aBsd = *Blanks
061400040401     c
061500040415      * Scrivo/Aggiorno Fidta
061600040415     c                   Eval      SavPoc = BtpFlp
061700040415     c
061800040401     c* Trasmetto DET  solo se cambiato
061900040401   2ac                   if        wdtadet<>btpdet
062000030206      * DET
062100030109     c                   Eval      wFidta = '0'
062200030108     c                   Eval      SavTrd = 'DET'
062300030109     c     kFidta        Setll     Fidta01l
062400030109     c                   Do        *Hival
062500030109     c     kFidta        Reade     Fidta01l
062600030109      * Fine file
062700030109     c                   If        %Eof(Fidta01l)
062800030109     c                   Leave
062900030109     c                   EndIf
063000030109      * Già trasmesso
063100030109     c                   If        DtaFtr <> *Blanks
063200030109     c                   Iter
063300030109     c                   EndIf
063400030109      * Aggiorno
063500030109     c                   Eval      DtaDta = BtpDet
063600030108     c                   Update    Fidta000
063700030109     c                   Eval      wFidta = '1'
063800030109     c                   EndDo
063900030109      * Scrivo
064000030109     c                   If        wFidta = '0'
064100030108     c                   Clear                   Fidta000
064200030108     c                   Eval      DtaAas = BtpAas
064300030108     c                   Eval      DtaLnp = BtpLnp
064400030108     c                   Eval      DtaNrs = BtpNrs
064500030108     c                   Eval      DtaNsp = BtpNsp
064600030108     c                   Eval      DtaTrd = SavTrd
064700030108     c                   Eval      DtaPoc = SavPoc
064800030109     c                   Eval      DtaDta = BtpDet
064900030108     c                   Write     Fidta000
065000030108     c                   EndIf
065100040401    2c                   EndIf
065200030108
065300040401      * DUT
065400040401     c
065500040401     c* Trasmetto DUT  solo se cambiato
065600040422   2ac***                if        wdtadut<>btpdut
065700030109     c                   Eval      wFidta = '0'
065800030108     c                   Eval      SavTrd = 'DUT'
065900030109     c     kFidta        Setll     Fidta01l
066000030109     c                   Do        *Hival
066100030109     c     kFidta        Reade     Fidta01l
066200030109      * Fine file
066300030109     c                   If        %Eof(Fidta01l)
066400030109     c                   Leave
066500030109     c                   EndIf
066600030109      * Già trasmesso
066700030109     c                   If        DtaFtr <> *Blanks
066800030109     c                   Iter
066900030109     c                   EndIf
067000030109      * Aggiorno
067100030109     c                   Eval      DtaDta = BtpDut
067200030108     c                   Update    Fidta000
067300030109     c                   Eval      wFidta = '1'
067400030109     c                   EndDo
067500030109      * Scrivo
067600030109     c                   If        wFidta = '0'
067700030108     c                   Clear                   Fidta000
067800030108     c                   Eval      DtaAas = BtpAas
067900030108     c                   Eval      DtaLnp = BtpLnp
068000030108     c                   Eval      DtaNrs = BtpNrs
068100030108     c                   Eval      DtaNsp = BtpNsp
068200030108     c                   Eval      DtaTrd = SavTrd
068300030108     c                   Eval      DtaPoc = SavPoc
068400030109     c                   Eval      DtaDta = BtpDut
068500030108     c                   Write     Fidta000
068600030108     c                   EndIf
068700040422   2ac***                EndIf
068800030320    2c                   EndIf
068900030108
069000031127    1C**                 ENDIF
069100031127     c
069200031127     C     KSAV          READE     FNBTP01L                             9934
069300031127    0c                   enddo
069400950130     C*
069500950130     C                   ENDSR
069600950130     C**************************************************************************
069700950130     C* AGGIORNO BOLLE  PARTENZA
069800950130     C**************************************************************************
069900950130     C     AGGBLP        BEGSR
070000111005     c
070100950130     C                   MOVEL     *HIVAL        WDPC
070200950926     C                   MOVEL     *HIVAL        WDSE
070300950130     C                   MOVEL     *HIVAL        WDET
070400950130     C                   MOVEL     *HIVAL        WDAM
070500950130     C                   Z-ADD     *ZERO         WDUT
070600950130     C                   Z-ADD     *ZERO         WDUC
070700041129     C                   MOVEA     *ZERO         Wnfv
070800951026     C                   SETOFF                                       99
070900950130     C*
071000950130     C     KSAV          SETLL     FNBLT01L
071100950130     C     KSAV          READE     FNBLT01L                               34
071200950130     C*
071300950130    1C     *IN34         DOWEQ     '0'
071400950130     C*
071500950130     C* CAUSALE PER AGGIORNAMENTO DATE
071600070504    2C     agbagb        IFEQ      'DT'
071700950130     C*
071800950130     C* DATA PARTENZA 1 COLLO
071900950130    3C     BLTDFV        IFGT      0
072000160725     c
072100950130    4C     BLTDFV        IFLT      WDPC
072200950130     C                   MOVEL     BLTDFV        WDPC
072300950130    4C                   END
072400950130     C* DATA PARTENZA ULTIMO COLLO
072500950130    4C     BLTDFV        IFGT      WDUC
072600950130     C                   MOVEL     BLTDFV        WDUC
072700041129     C                   MOVEL     BLTnFV        Wnfv(1)
072800041129     c                   else
072900041129     c* memorizzo se stessa data ma <> foglio
073000041129     c                   if        bltdfv=wduc and bltdfv>0 and
073100041129     c                             bltnfv<>wnfv(1)
073200041129     c                   eval      wnfv(2)=bltnfv
073300041129     c                   endif
073400041129     c
073500950130    4C                   END
073600950130     C*
073700950130   X3C                   ELSE
073800950130     C                   Z-ADD     *HIVAL        WDUC
073900950130    3C                   END
074000950926     C* DATA SPUNTA ENTRATA
074100950926    3C     BLTDSE        IFGT      0
074200950926    4C     BLTDSE        ANDLT     WDSE
074300950926     C                   MOVEL     BLTDSE        WDSE
074400950926    4C                   ENDIF
074500950926     C* DATA ENTRATA TRANSITO
074600950926    3C     BLTDET        IFGT      0
074700950926    4C     BLTDET        ANDLT     WDET
074800950926     C                   MOVEL     BLTDET        WDET
074900950926    4C                   ENDIF
075000950926     C* DATA USCITA TRANSITO
075100950926    3C     BLTDUT        IFGT      0
075200950926    4C     BLTDUT        ANDLT     WDUT
075300950926     C                   MOVEL     BLTDUT        WDUT
075400950926    4C                   ENDIF
075500960520     C* DATA ARRIVO
075600960520    3C     BLTDAM        IFGT      0
075700960520    4C     BLTDAM        ANDLT     WDAM
075800960520     C                   MOVEL     BLTDAM        WDAM
075900960520    4C                   ENDIF
076000950130     C*
076100950130    2C                   ENDIF
076200950130     C*
076300950130     C     KSAV          READE     FNBLT01L                               34
076400950130    1C                   ENDDO
076500950130     C*
076600950130     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
076700950130     C*   TRANSITO E FILIALE TRANSITO
076800951026     C     KSAV          CHAIN     FNBLP01L                           3099
076900950130     C*
077000951026    1C     *IN30         IFEQ      *OFF
077100951026     C     *IN99         ANDEQ     *OFF
077200091021     c                   eval      wdtaduc=blpduc
077300950130     C* DATA PARTENZA ULTIMO COLLO
077400950130    2C     WDUC          IFNE      *HIVAL
077500950130     C                   MOVEL     WDUC          BLPDUC
077600160726     c
077700160726     c* Se richiesto verifico RIELABORAZIONE STATISTICHE PARTENZE
077800160726     c                   if        §agbdsf='S'
077900160726     c                   clear                   fnlsu2r3ds
078000160726     c                   eval      sa2trc='E'
078100160726     c                   eval      sa2dts=wduc
078200160726     c                   eval      sa2ncl=blpncl
078300160726     c                   eval      sa2tfp=blptfp
078400160726     c                   call      'FNLSU2R3'
078500160726     c                   parm                    fnlsu2r3ds
078600160726     c                   eval      wopenlsu2='S'
078700160726
078800160726     c                   if        wdpc<>wduc and wdpc<>*hival
078900160726     c                   clear                   fnlsu2r3ds
079000160726     c                   eval      sa2trc='E'
079100160726     c                   eval      sa2dts=wdPC
079200160726     c                   eval      sa2ncl=blpncl
079300160726     c                   eval      sa2tfp=blptfp
079400160726     c                   call      'FNLSU2R3'
079500160726     c                   parm                    fnlsu2r3ds
079600160726     c                   endif
079700160726     c                   endif
079800160726     c
079900041126     c* Se foglio borderizzazione <> da foglio aggiornamento DUC
080000041126     c*  e bolla già inviata, scrivo DTA per DUC
080100041129     c                   if        blpft1<>' '
080200041129     c                   if        (wnfv(1)<>blpnfv and wnfv(1)>0) or
080300041129     c                             (wnfv(2)<>blpnfv and wnfv(2)>0)
080400041129     c                   EXSR      SCRIVIduc
080500091021     c                   else
080600091021     c* Se il foglio è quello di borderizzazione ma la bolla non è più nel
080700091021     c*  fnblp46f, devo trasmettere lo stesso
080800091021     c     kblp46        chain     fnblp46l
080900091021     c                   if        not %found(fnblp46l)
081000091021     c                   EXSR      SCRIVIduc
081100041126     c                   endif
081200091021     c
081300091021     c                   endif
081400041129     c                   endif
081500041126     c
081600950130     C                   ELSE
081700950130     C                   MOVEL     *ZEROS        BLPDUC
081800950130    2C                   END
081900950130     C* DATA PARTENZA PRIMO COLLO
082000950130    2C     WDPC          IFNE      *HIVAL
082100950130     C                   MOVEL     WDPC          BLPDPC
082200950130     C                   ELSE
082300950130     C                   MOVEL     *ZEROS        BLPDPC
082400950130    2C                   END
082500950926     C* DATA SPUNTA ENTRATA
082600950926    2C     WDSE          IFNE      *HIVAL
082700950926     C                   MOVEL     WDSE          BLPDSE
082800950926     C                   ELSE
082900950926     C                   MOVEL     *ZEROS        BLPDSE
083000950926    2C                   END
083100950926     C* DATA ENTRATA TRANSITO
083200950926    2C     WDET          IFNE      *HIVAL
083300950926     C                   MOVEL     WDET          BLPDET
083400950926     C                   ELSE
083500950926     C                   MOVEL     *ZEROS        BLPDET
083600950926    2C                   END
083700950926     C* DATA USCITA TRANSITO
083800950926    2C     WDUT          IFNE      *HIVAL
083900950926     C                   MOVEL     WDUT          BLPDUT
084000950926     C                   ELSE
084100950926     C                   MOVEL     *ZEROS        BLPDUT
084200950926    2C                   END
084300960520     C* DATA ARRIVO
084400960520    2C     WDAM          IFNE      *HIVAL
084500960520     C                   MOVEL     WDAM          BLPDAM
084600960520     C                   ELSE
084700960520     C                   MOVEL     *ZEROS        BLPDAM
084800960520    2C                   END
084900951122     C* VOLUME CML
085000070504     C     agbfvc        IFEQ      'S'
085100951122     C                   Z-ADD     99999         BLPNCR
085200951122     C                   ENDIF
085300990728     C* PESO CML
085400070504     C     agbfpc        IFEQ      'S'
085500990728     C                   Z-ADD     99999         BLPNCP
085600990728     C                   ENDIF
085700950130     C*
085800950130     C                   UPDATE    FNBLP000
085900950130    1C                   ENDIF
086000950130     C*
086100950130     C                   ENDSR
086200950130     C**************************************************************************
086300950130     C* DELETO I RECORD IN FNAGB00F DOPO AVER AGGIORNATO
086400950130     C**************************************************************************
086500950130     C     DELET         BEGSR
086600040309     c                   clear                   werror
086700950130     C*
086800951026     C* NON CANCELLO SE HO TROVATO L'ERRORE
086900040309    1C     *IN99         IFEQ      *OFF
087000070507     c     fnagbnrr      chain(e)  fnagb00f
087100031016     C*
087200031016     C* Gestisco eventuali errori d allocazine record x i quali tramite il
087300031016     C* *PGM d utilità TRUL82R scelgo d inviare 1 MSG alla coda dell'operatore
087400040309    2C                   IF        %error
087500031016     C*
087600031016     C* Valorizzo la DS di procedura
087700031016     C                   CLEAR                   TRUL82DS
087800070507     c                   eval      ul82§rrn = fnagbnrr
087900031016     C                   EVAL      UL82§FIL = 'FNAGB00F'
088000031016     C                   EVAL      UL82§WIN = 'N'
088100031016     C                   EVAL      UL82§F5  = 'S'
088200031016     C*
088300031016     C* Effettuo la chiamata al *PGM d utilità
088400031016     C                   CALL(e)   'TRUL82R'
088500031016     C                   PARM                    TRUL82DS
088600031016     C*
088700040309     c*** Se trovato record allocato vado a fine pgm
088800040309     c                   movel     'E'           werror            1
088900040309     c                   goto      enddel
089000040309    2C                   ENDIF
089100950130     C*
089200070507     c                   if        %found(fnagb00f)
089300070507     C                   DELETE    FNAGBF00
089400070507     c                   endif
089500040309    3C                   ENDIF
089600040309     C     enddel        ENDSR
089700950130     C**************************************************************************
089800950130     C* ELABORO LE BOLLE E AGGIORNO
089900950130     C**************************************************************************
090000950130     C     ELAB          BEGSR
090100110701     c                   eval      $err='0'
090200160726     c                   eval      dagbflo=agbflo
090300950130     C*
090400950130     C                   SELECT
090500160422     c* verifico se la causale è tutta numerica
090600160422     c*  --> aggiornamento della zona sulla bolla
090700160422     c                   when      %check(digits:agbagb)=0
090800160422     c                   EXSR      AGG_zona
090900110701     C* BOLLE ARRIVI flag inoltro
091000110701     C     agbtbo        WHENEQ    'A'
091100110701     C     agbagb        andeq     'ST'
091200110701     C                   EXSR      A_arbfin
091300110701     C* BOLLE PARTENZE flag inoltro
091400110701     C     agbtbo        WHENEQ    'P'
091500110701     C     agbagb        andeq     'ST'
091600110701     C                   EXSR      A_blpfin
091700111007     C* BOLLE SEDE     flag inoltro
091800111007     C     agbtbo        WHENEQ    'S'
091900111007     C     agbagb        andeq     'ST'
092000111007     C                   EXSR      A_tasfin
092100150604     C* BOLLE PARTENZE cambia LNP Bolla per clienti in file FIBSP
092200150604     C     agbtbo        WHENEQ    'P'
092300150604     C     agbagb        andeq     'CB'
092400150604     C                   EXSR      P_blpLNPB
092401170908     C* BOLLE PARTENZE ripristino Bolla chiusa per merce mai affidata
092402170908     C     agbtbo        WHENEQ    'P'
092403170908     C     agbagb        andeq     'RB'
092404170908     C                   EXSR      P_blpRipr
092405180122     c* BOLLE ARRIVI Aggiunta/delete info di incompatibile su titah e titas
092406180123     C                   WHEN      agbtbo='A' and %subst(agbagb:1:1)='J'
092407180122     c                   exsr      agg_inc
092500950130     C* BOLLE ARRIVI
092600070504     C     agbtbo        WHENEQ    'A'
092700950130     C                   EXSR      AGGARB
092800950130     C* BOLLE PARTENZE
092900070504     C     agbtbo        WHENEQ    'P'
093000950130     C                   EXSR      AGGBLP
093100950130     C* BOLLE TRANSITO
093200070504     C     agbTBO        WHENEQ    'T'
093300950130     C                   EXSR      AGGBTP
093400950130     C                   ENDSL
093500950130     C*
093600110701     C* DELETO I RECORD senon c'e' stato errore in allocaizone record
093700150608     c                   if        $err='0'
093800950130     C                   EXSR      DELET
093900110701     c                   endif
094000950130     C*
094100950130     C                   ENDSR
094200041104     C**************************************************************************
094300041104     C* ELABORA BOLLE PER AGGIORNARE IL PESO CML
094400041104     C**************************************************************************
094500041104     C     ELAAGP        BEGSR
094600041108     C     1             SETLL     FNAGP00f
094700041108     C                   READ      FNAGP00f
094800041108     C                   DOW       NOT %EOF(FNAGP00f)
094900041104     c
095000041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
095100041108     c*  non penso sarà mai attivabile
095200041108     c                   if        agptbo='P'
095300041104     C     KAgp          CHAIN     FNblp01L                           38
095400041104     c  n38              exsr      APPMOPP
095500070705     c                   if        d39err=*blanks  and o57err=*blanks
095600041108     c                   delete    fnagp000
095700041108     c                   endif
095800041108     c                   endif
095900041108     c
096000041108     C                   READ      FNAGP00f
096100041104     C                   ENDDO
096200111005     c
096300111005     c* Chiudo file  del pgm richiamato
096400111005     c                   clear                   fnlv39ds
096500111005     c                   eval      d39tla='C'
096600111005     C                   MOVEL     fnlv39ds      KPJBU
096700111005     c                   call      'FNLV39R'
096800111005     C                   PARM                    KPJBA
096900111005     c*
097000041104     C                   ENDSR
097100041108     C**************************************************************************
097200041108     C* ELABORA BOLLE PER AGGIORNARE IL volume cML
097300041108     C**************************************************************************
097400041108     C     ELAAGV        BEGSR
097500041108     C     1             SETLL     FNAGV00f
097600041108     C                   READ      FNAGV00f
097700041108     C                   DOW       NOT %EOF(FNAGV00f)
097800041108     c
097900041108     c* Per ora prevedo solo di ricevere in partenza il caso contrario
098000041108     c*  non penso sarà mai attivabile
098100041108     c                   if        agvtbo='P'
098200041108     C     KAgv          CHAIN     FNblp01L                           38
098300041108     c  n38              exsr      APPMOVP
098400070705     c                   if        d20err=*blanks  and o57err=*blanks
098500041108     c                   delete    fnagV000
098600041108     c                   endif
098700041108     c                   endif
098800041108     c
098900041108     C                   READ      FNAGV00f
099000041108     C                   ENDDO
099100111005     c
099200111005     c* Chiudo file  del pgm richiamato
099300111005     c                   clear                   fnlv20ds
099400111005     c                   eval      d20tla='C'
099500111005     C                   MOVEL     fnlv20ds      KPJBU
099600111005     c                   call      'FNLV20R'
099700111005     C                   PARM                    KPJBA
099800111005     c
099900041108     C                   ENDSR
100000041104     C*---------------------------------------------------------------*
100100041104     C* ROUTINE DI APPLICAZIONE MODIFICHE PESO A BOLLA PARTENZA
100200041104     C*---------------------------------------------------------------*
100300041104     C     APPMOPP       BEGSR
100400041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
100500041104     C                   Z-ADD     BLPPKF        OLDPKF
100600041104     C                   Z-ADD     BLPPKC        OLDPKC
100700041104     C                   Z-ADD     BLPNCP        OLDNCP
100800041108     C                   Z-ADD     BLPvlF        OLDvlf
100900041108     C                   Z-ADD     BLPvlc        OLDvlc
101000041108     C                   Z-ADD     BLPNCr        OLDNCr
101100070705     c                   movel     blpfvf        OLDfvf
101200041104     C*
101300041108     C                   CLEAR                   fnlv39ds
101400041104     C                   MOVEL     'P'           D39TBO
101500041108     C                   MOVEL     'C'           D39TPP
101600041104     C                   Z-ADD     BLPPKB        D39PLB
101700041104     C                   Z-ADD     BLPPKF        D39PLF
101800041108     C                   Z-ADD     agpPKC        D39PLU
101900041108     C                   Z-ADD     agpNCP        D39NCP
102000041104     C                   Z-ADD     BLPAAS        D39AAS
102100041104     C                   Z-ADD     BLPNRS        D39NRS
102200041104     C                   Z-ADD     BLPLNP        D39LNP
102300041104     C                   Z-ADD     BLPNSP        D39NSP
102400041104     C                   MOVEL     BLPCBO        D39CBO
102500041104     C                   Z-ADD     BLPLNA        D39LNA
102600041104     C                   Z-ADD     BLPNCL        D39NCL
102700041104     C                   Z-ADD     *date         D39DTE
102800041108     C                   MOVEL     fnlv39ds      KPJBU
102900041104     C                   CALL      'FNLV39R'
103000041104     C                   PARM                    KPJBA
103100041108     C                   MOVEL     KPJBU         fnlv39ds
103200041104     C*
103300041104     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
103400041104    3C     D39ERR        IFNE      '1'
103500041104     C                   MOVEL     D39NCP        BLPNCP
103600041104     C                   MOVEL     D39PLU        BLPPKC
103700041104     C                   MOVEL     D39PLF        BLPPKF
103800041104     C* AGGIORNO FILE TASSAZIONE PADRONCINI
103900041108     C                   CLEAR                   fnlv57ds
104000041104     C                   MOVEL     'P'           I57TBO
104100041104     C                   EXSR      AGGTAS
104200070705     c                   if        o57err<>' '
104300070705     C                   MOVEL     OLDNCp        blpNCp
104400070705     C                   MOVEL     OLDpkc        blppkc
104500070705     C                   MOVEL     OLDpkf        blppkf
104600070705     c                   endif
104700070705     c
104800041104     C                   UPDATE    FNBLP000
104900041104E   2C                   ENDIF
105000041104     C*
105100041104     C                   ENDSR
105200041108     C*---------------------------------------------------------------*
105300041108     C* ROUTINE DI APPLICAZIONE MODIFICHE volume A BOLLA PARTENZA
105400041108     C*---------------------------------------------------------------*
105500041108     C     APPMOVP       BEGSR
105600041108     C* SALVO I CAMPI VECCHI DEL PESO e volume
105700041108     C                   Z-ADD     BLPPKF        OLDPKF
105800041108     C                   Z-ADD     BLPPKC        OLDPKC
105900041108     C                   Z-ADD     BLPNCP        OLDNCP
106000041108     C                   Z-ADD     BLPvlF        OLDvlf
106100041108     C                   Z-ADD     BLPvlc        OLDvlc
106200041108     C                   Z-ADD     BLPNCr        OLDNCr
106300070705     c                   movel     blpfvf        OLDfvf
106400041108     C*
106500041108     C                   CLEAR                   fnlv20ds
106600041108     C                   MOVEL     'P'           D20TBO
106700041108     C                   MOVEL     'C'           D20tvl
106800041108     C                   MOVEL     BLPFVB        D20FVB
106900041108     C                   Z-ADD     BLPVLB        D20VLB
107000041108     C                   MOVEL     BLPFVF        D20FVF
107100041108     C                   Z-ADD     BLPVLF        D20VLF
107200041108     C                   Z-ADD     agVVLC        D20VLU
107300041108     C                   Z-ADD     agVNCR        D20NCR
107400041108     C                   Z-ADD     BLPAAS        D20AAS
107500041108     C                   Z-ADD     BLPNRS        D20NRS
107600041108     C                   Z-ADD     BLPLNP        D20LNP
107700041108     C                   Z-ADD     BLPNSP        D20NSP
107800041108     C                   MOVEL     BLPCBO        D20CBO
107900041108     C                   Z-ADD     BLPLNA        D20LNA
108000041108     C                   Z-ADD     BLPNCL        D20NCL
108100041108     C                   Z-ADD     *date         D20DTE
108200041108     C                   MOVEL     fnlv20ds      KPJBU
108300041108     C                   CALL      'FNLV20R'
108400041108     C                   PARM                    KPJBA
108500041108     c                   parm      'N'           pwstor            1
108600041108     C                   MOVEL     KPJBU         fnlv20ds
108700041108     C*
108800041108     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
108900041108    3C     D20ERR        IFNE      '1'
109000041108     C                   MOVEL     D20NCR        BLPNCR
109100041108     C                   MOVEL     D20VLU        BLPVLC
109200041108     C                   MOVEL     D20FVF        BLPFVF
109300041108     C                   MOVEL     D20VLF        BLPVLF
109400041108     C* AGGIORNO FILE TASSAZIONE PADRONCINI
109500041108     C                   CLEAR                   fnlv57ds
109600041108     C                   MOVEL     'P'           I57TBO
109700041108     C                   EXSR      AGGTAS
109800070705     c                   if        o57err<>' '
109900070705     C                   MOVEL     OLDNCR        blpNCR
110000070705     C                   MOVEL     OLDvlc        blpVLC
110100070705     C                   MOVEL     OLDFVF        blpFVF
110200070705     C                   MOVEL     OLDVLF        blpVLF
110300070705     c                   endif
110400070705     c
110500041108     C                   UPDATE    FNBLP000
110600041108E   2C                   ENDIF
110700041108     C*
110800041108     C                   ENDSR
110900041104     C*---------------------------------------------------------------*
111000041104     C* AGGIORNA TASSAZIONE PADRONCINI
111100041104     C*---------------------------------------------------------------*
111200041104     C     AGGTAS        BEGSR
111300041104     C                   MOVEL     blpAAS        I57AAS
111400041104     C                   MOVEL     blpLNP        I57LNP
111500041104     C                   MOVEL     blpNRS        I57NRS
111600041104     C                   MOVEL     blpNSP        I57NSP
111700041104     C                   MOVEL     BLPDRT        I57DRT
111800041104     C                   MOVEL     BLPNRT        I57NRT
111900041104     C                   MOVEL     BLPFPP        I57FPP
112000041104     C                   MOVEL     BLPNCL        I57NCL
112100041104     C                   MOVEL     BLPPDR        I57PDR
112200041108     c* Campi nuovi modificati
112300041104     C                   MOVEL     BLPPKC        I57Pcn
112400041104     C                   MOVEL     BLPncp        I57npn
112500041104     C                   MOVEL     BLPPKf        I57Pfn
112600041108     C                   MOVEL     BLPVLC        I57VCN
112700041108     C                   MOVEL     BLPNCR        I57NCN
112800041108     C                   MOVEL     BLPVLF        I57VFN
112900041108     c* Campi della bolla prima dellamodifica
113000041104     C                   MOVEL     OLDPKC        I57pkc
113100041104     C                   MOVEL     OLDncp        I57ncp
113200041104     C                   MOVEL     OLDPKF        I57PKF
113300041108     C                   MOVEL     OLDVLC        I57VLC
113400041108     C                   MOVEL     OLDNCR        I57NCR
113500041108     C                   MOVEL     OLDVLF        I57VLF
113600041104     C                   MOVEL     'C'           I57TVL
113700041104     C                   CALL      'FNLV57R'
113800041104     C                   PARM                    KPJBA
113900041108     C                   PARM                    fnlv57ds
114000111005     c                   eval      ApriLV57='S'
114100041104     C                   ENDSR
114200041126     C*---------------------------------------------------------------*
114300041126     C* scrivi fidta00f per data partenza ultimo collo
114400041126     C*---------------------------------------------------------------*
114500041129     C     ScriviDUC     BEGSR
114600041129     c* Scrivo DTA solo se cambiata la data
114700041129    1c                   if        wdtaduc<>blpduc
114800041129      * Se è un tipo bolla da trasmettere in sede
114900041129     c                   Clear                   ds3a
115000041129     c                   Z-Add     1             Codut
115100041129     c                   Eval      Cod = '3A'
115200041129     c                   Clear                   Key
115300041129     c                   Movel     blpCbo        Key
115400041129     c     kTab          Chain     Tabel00f
115500041129     c                   If        %Found(Tabel00f)
115600041129     c                   Movel     TblUni        ds3a
115700041129     c                   EndIf
115800041129If  2c                   If        §3aBsd = *Blanks
115900041129      * Scrivo/Aggiorno Fidta
116000041129     c                   Eval      wFidta = '0'
116100041129     c                   Eval      SavPoc = blplnp
116200041129     c                   Eval      SavTrd = 'DUC'
116300041129     c     kFidta        Setll     Fidta01l
116400041129     c                   Do        *Hival
116500041129     c     kFidta        Reade     Fidta01l
116600041129      * Fine file
116700041129     c                   If        %Eof(Fidta01l)
116800041129     c                   Leave
116900041129     c                   EndIf
117000041129      * Già trasmesso
117100041129     c                   If        DtaFtr <> *Blanks
117200041129     c                   Iter
117300041129     c                   EndIf
117400041129      * Aggiorno
117500041129     c                   Eval      DtaDta = blpduc
117600041129     c                   Update    Fidta000
117700041129     c                   Eval      wFidta = '1'
117800041129     c                   EndDo
117900041129      * Scrivo
118000041129     c                   If        wFidta = '0'
118100041129     c                   Clear                   Fidta000
118200041129     c                   Eval      DtaAas = blpAas
118300041129     c                   Eval      DtaLnp = blpLnp
118400041129     c                   Eval      DtaNrs = blpNrs
118500041129     c                   Eval      DtaNsp = blpNsp
118600041129     c                   Eval      DtaTrd = SavTrd
118700041129     c                   Eval      DtaPoc = SavPoc
118800041129     c                   Eval      DtaDta = blpduc
118900041129     c                   Write     Fidta000
119000041129     c                   EndIf
119100041129    2c                   EndIf
119200041129    1c                   EndIf
119300041126     C                   ENDSR
119400110701     C**************************************************************************
119500110701     C* AGGIORNO flag Inoltro bolla arrivi
119600110701     C**************************************************************************
119700110701     C     A_arbfin      BEGSR
119800110701     C*
119900110701     C     KSAV          chain(e)  FNARB01L
120000110701     c                   if        %error
120100110701     c                   eval      $err='1'
120200110701     c                   leavesr
120300110701     c                   endif
120400110701     c
120500110706     c                   if        %found(fnarb01l)  and arbfin<>'I' and
120600110706     c                             arbfin<>'D'
120700110701     c                   eval      arbfin=agbfbs
120800110701     c                   update    fnarb000
120900110706     c                   else
121000110706     c                   unlock    fnarb01l
121100110701     c                   endif
121200110701     c
121300110701     C                   ENDSR
121400110701     C**************************************************************************
121500110701     C* AGGIORNO flag Inoltro bolla Partenza
121600110701     C**************************************************************************
121700110701     C     A_blpfin      BEGSR
121800110701     C*
121900110701     C     KSAV          chain(e)  FNBLP01L
122000110701     c                   if        %error
122100110701     c                   eval      $err='1'
122200110701     c                   leavesr
122300110701     c                   endif
122400110701     c
122500110706     c                   if        %found(fnblp01l) and blpfin<>'I' and
122600110706     c                             blpfin<>'D'
122700110701     c                   eval      BLPfin=agbfbs
122800110701     c                   update    fnblp000
122900110706     c                   else
123000110706     c                   unlock    fnblp01l
123100110701     c                   endif
123200110701     c
123300110701     C                   ENDSR
123400111007     C**************************************************************************
123500111007     C* AGGIORNO flag Inoltro bolla Sede
123600111007     C**************************************************************************
123700111007     C     A_tasfin      BEGSR
123800111007     C*
123900111007     c                   setoff                                       020304
124000130124     c                   clear                   wagg1
124100111007     C     KSAV          chain(e)  titas30c
124200111007     c
124300111011    1c                   if        %error or not %found(titas30c)
124400111007     c                   eval      $err='1'
124500111007     c                   leavesr
124600111011    1c                   endif
124700111007     c
124800111011    1c                   if        %found(titas30c)
124900111011
125000111011    2c                   if        *in02  and tasfic=' '  and tasfin<>'D'
125100111007     c                             and tasfin<>'I'
125200111007     c                   eval      tasfin=agbfbs
125300111007     c                   update    titas000
125400130124     c                   eval      wagg1='S'
125500111011     c
125600111011     c* cerco se c'e' seconda bolla
125700111011     c                   setoff                                       020304
125800111011     C     KSAV          reade(e)  titas30c
125900111011     c
126000111011    3c                   if        %error
126100111011     c                   eval      $err='1'
126200111011     c                   leavesr
126300111011    3c                   endif
126400111011     c                   if        not %eof(titas30c) and
126500111011    3c                             *in02  and tasfic=' '  and tasfin<>'D'
126600111011     c                             and tasfin<>'I'
126700111011     c                   eval      tasfin=agbfbs
126800111011     c                   update    titas000
126900111011    3c                   endif
127000111011    c
127100111011    2c                   endif
127200130124     c* Già contabilizzata - I bolla
127300130124     c* se inoltro non aggiornato perchè bolla già contabilizzata
127400130124     c* bisogna scrivere file per recupero variazioni post-fatturazione
127500130305     c* Non scrivo mai tivrb per la seconda bolla in quanto il flag
127600130305     c* inoltro non influisce sulla tassazione della seconda bolla
127700130305    2c                   if        wagg1=*blanks and tasfin<>'D' and
127800130305     c                                               tasfin<>'I'
127900130305     c                   exsr      sr_tivrb
128000130305    2c                   endif
128100111011    1c                   endif
128200111007
128300111007     c                   unlock    titas30c
128400111007     c
128500111007     C                   ENDSR
128600160422     C**************************************************************************
128700160422     C* AGGIORNO zona  bolla Partenza
128800160422     C**************************************************************************
128900160422     C     AGG_zona      BEGSR
129000160422     C*
129100160422     C     KSAV          chain(e)  FNBLP01L
129200160422     c                   if        %error
129300160422     c                   eval      $err='1'
129400160422     c                   leavesr
129500160422     c                   endif
129600160422     c
129700160422     c                   if        %found(fnblp01l)
129800160422     c                   movel     agbagb        blpznc
129900160422     c                   update    fnblp000
130000160422     c                   endif
130100160422
130200160422     c* Se fnblp trasmesso aggiorno anche FNARB
130300160505     c                   if        blpft1<>'N' and blpft1<>' '
130400160422     C     KSAV          chain(e)  FNARB01L
130500160422     c                   if        %error
130600160422     c                   eval      $err='1'
130700160422     c                   leavesr
130800160422     c                   endif
130900160422     c
131000160422     c                   if        %found(fnarb01l)
131100160422     c                   movel     agbagb        arbznc
131200160422     c                   update    fnarb000
131300160422     c                   endif
131400160422
131500160422     c                   endif
131600160422     c
131700160422     C                   ENDSR
131800150604     C**************************************************************************
131900150604     C* AGGIORNO lnp Bolla per collo di cliente che parte da altra filiale
132000150604     C**************************************************************************
132100150604     C     P_blpLNPB     BEGSR
132200150609     C     KSAV          chain(n)  FNBLP01L
132300150604     c* se la bolla manca è già stata probabilmente "presa" da un'altra filiale
132400150604     c* per cui non devo fare nulla
132500150605    1c                   if        %found(fnblp01l)
132600150605     c* se presente data entrata non devo fare niente
132700150605    2c                   if        blpdse>0
132800150604     c                   leavesr
132900150605    2c                   endif
133000150605     c                   exsr      sr_fibsp
133100150608     c                   if        wBspTp<>'K' and wBspTp<>'L'
133200150608     c                   leavesr
133300150608     c                   endif
133400150604     c* lettura delle spunte per determinare la nuova lnp bolla
133500150608     c                   clear                   wlnpb
133600150608     c                   move      *hival        wdcs
133700150608     c                   clear                   whcs
133800150608     c                   clear                   wdfv
133900150608     c* segnacolli non sequenziali
134000150608    2c                   if        blpfns='S'
134100150608     c     ksav          setll     fnblt01l
134200150608     c     ksav          reade     fnblt01l
134300150608    3c                   dow       not %eof(fnblt01l)
134400150608     c                   z-add     bltnsc        wncd
134500150608     c                   exsr      sr_spunte
134600150608     c     ksav          reade     fnblt01l
134700150608    3c                   enddo
134800150608     c* segnacolli sequenziali
134900150608   x2c                   else
135000150605     c                   eval      wncd=blpncd
135100150605     c                   eval      wnca=blpnca
135200150608    3c                   if        wnca=0
135300150605     c                   eval      wnca=blpncd
135400150608    3c                   endif
135500150608    3c                   dow       wncd<=wnca
135600150608     c                   exsr      sr_spunte
135700150605     c                   add       1             wncd
135800150608    3c                   enddo
135900150608    2c                   endif
136000150625    2c                   if        wBspTp='L'
136100150605     c     wlnpb         chain     azorg01l
136200150605     c                   movel     orgde3        og143
136300150625    3c                   if        §oglnpb>*zeros
136400150605     c                   move      §OGLNPB       wlnpb
136500150625   x3c                   else
136600150624     c* se su azorg la lnp valida come lnp bolla è vuota prendo da azcae la
136700150625     c* prima filiale dpd del terminal
136800150625     C                   CLEAR                   TRUL06DS
136900150625     C                   MOVE      'PP'          D06COD
137000150625     C                   MOVEL     wlnpb         D06KEY
137100150625     c                   move      'S'           d06esc
137200150625     C                   MOVEL     trul06ds      KPJBU
137300150625     c                   call      'TRUL06R'
137400150625     C                   PARM                    KPJBA
137500150625     c                   movel     kpjbu         trul06ds
137600150625     c* loop su schiera delle filiali
137700150625     c                   z-add     1             idx
137800150625     c                   clear                   wlnpb
137900150625     c                   dow       idx<=30  and l06(idx)>0 and wlnpb=0
138000150625     c     l06(idx)      chain     azorg01l
138100150625     c                   if        %found(azorg01l)
138200150625     c                   movel     orgde3        og143
138300150625    3c                   if        §ogntw='DPD'
138400150625     c                   move      l06(idx)      wlnpb
138500150625     c                   endif
138600150625     c                   endif
138700150625     c                   add       1             idx
138800150625     c                   enddo
138900150625     c
139000150625    3c                   endif
139100150624     c
139200150625    2c                   endif
139300150608     c                   if        wlnpb>0 and wlnpb<>blplnp
139400150605     c* richiamo pgm di modifica linea di partenza bolla su archivio bolle part
139500150605     c* e file collegati
139600150605     c                   clear                   fnls65ds
139700150605     c                   z-add     blpaas        I65AAS
139800150605     c                   z-add     blplnp        I65LNP
139900150605     c                   z-add     blpnrs        I65NRS
140000150605     c                   z-add     blpnsp        I65NSP
140100150605     c                   z-add     wlnpb         I65lnpn
140200150605     c                   movel     d53dfv        I65AASN
140300150605     c                   move      D53dfv        I65MGSN
140400150605     c                   movel     'S'           I65AGDRT
140500150605     c
140600150605     c                   move      '1'           I65STRCMT
140700150605     c                   move      '1'           I65FCMT
140800150605     c                   move      '1'           I65CMTRLBK
140900150605     c                   movel     fnls65ds      kpjbu
141000150605     c                   call      'FNLS65R'
141100150605     c                   parm                    kpjba
141200150605     c                   movel     kpjbu         fnls65ds
141300150824     c                   if        o65err='6' or
141400150824     c                             o65err='1' or
141500150824     c                             o65err='2' or
141600150824     c                             o65err='3'
141700150609     c                   eval      $err='1'
141800150609     c                   endif
141900150605     c                   endif
142000150608    1c                   endif
142100150604     c                   endsr
142101170908     C**************************************************************************
142102170908     C* Ripristino automatico bolla chiusa per merce mai aff.
142103170908     C**************************************************************************
142104170908     C     P_blpRipr     BEGSR
142105170908     C     KSAV          chain(n)  FNBLP01L
142108170908    1c                   if        %found(fnblp01l)
142109170908     c                             and blpdt1>0 and blpcca='7'
142110170908     c                   clear                   fnlsi4ds1
142111170908     c                   eval      II4AAS     =  blpaas
142112170908     c                   eval      II4LNP     =  blplnp
142113170908     c                   eval      II4NRS     =  blpnrs
142114170908     c                   eval      II4NSP     =  blpnsp
142115170908     c                   eval      II4STRCMT  =  '1'
142116170908     c                   eval      II4FCMT    =  '1'
142117170908     c                   eval      II4CMTRLBK =  '1'
142118170908     c                   movel     fnlsi4ds1     kpjbu
142119170908     c                   call      'FNLSI4R1'
142120170908     c                   parm                    kpjba
142121170908     c                   movel     kpjbu         fnlsi4ds1
142122170908     c                   if        oi4err<>*blanks
142125170908     c                   eval      $err='1'
142126170908     c                   endif
142203170908    1c                   endif
142204170908     c                   endsr
142205150608     C**************************************************************************
142300150608     C*  Lettura spunte per determinare la lnp bolla
142400150608     C**************************************************************************
142500150608     C     sr_spunte     BEGSR
142600150608     c     kbrv07        setll     fnbrv07l
142700150608     c     kbrv07        reade     fnbrv07l
142800150608    1c                   dow       not %eof(fnbrv07l)
142900150608     c* elaboro solo se  blptfp<>brvfle
143000150608     c* elaboro solo se la spunta è stata fatta da una delle filiali abilitate
143100150608    4c***                if        blptfp<>brvfle  and
143200150608    2c                   if        %lookup(brvpes:sbsp)>0
143300150608     c                             and (brvdcs<wdcs or (brvdcs=wdcs  and
143400150608     c                                  brvhcs<whcs))
143500150608     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
143600150608     c                   clear                   dslv53
143700150608     C                   z-add     brvnFV        D53NFV
143800150608     C                   MOVEL     brvNPG        D53NPG
143900150608     C                   MOVEL     brvFGS        D53FGS
144000150608     C                   CALL      'FNLV53R'
144100150608     C                   PARM                    DSLV53
144200150608     c* memorizzo
144300150608     c                   eval      wlnpb=brvpes
144400150608     c                   eval      wdcs=brvdcs
144500150608     c                   eval      whcs=brvhcs
144600150608     c                   eval      wdfv=d53dfv
144700150608    2c                   endif
144800150608     c     kbrv07        reade     fnbrv07l
144900150608    1c                   enddo
145000150608     c                   endsr
145100150605     C**************************************************************************
145200150605     C*
145300150605     C**************************************************************************
145400150605     C     sr_fibsp      BEGSR
145500150605     c                   eval      WBspTp=*blanks
145600150605    2c                   if        blpccm>0
145700150605     c                   eval      w_clibo=blpccm
145800150605     c                   else
145900150605     c                   eval      w_clibo=blpksc
146000150605    2c                   endif
146100150605     c     w_clibo       chain     fibsp02l
146200150608     c                   if        %found(fibsp02l) and bspfpa='S'
146300150608     c                   eval      wBspTp='K'
146400150608     c****               if        not %found(fibsp02l) or bspfpa<>'S'
146500150608     c                   else
146600150605     c     blplnp        chain     fibsp01l
146700150605     c                   if        %found(fibsp01l) and bspfpa='S'
146800150605     c                   eval      wBspTp='L'
146900150605     c                   endif
147000150605     c                   endif
147100150605     c                   endsr
147200130201     C**************************************************************************
147300130201     C* Richiamo pgm gestione file recupero variazioni post-fatturaz. (TIVRB)
147400130201     C**************************************************************************
147500130201     C     sr_tivrb      BEGSR
147600130201     c                   clear                   fnlvv1ds
147700130201     c                   eval      iv1cvb='ZT'
147800130201     c                   eval      iv1pgm=nompgm
147900130201     c                   eval      iv1aas=tasaas
148000130201     c                   eval      iv1lnp=taslnp
148100130201     c                   eval      iv1nrs=tasnrs
148200130201     c                   eval      iv1nsp=tasnsp
148300130201     c                   eval      iv1tbl=tastbl
148400130201     c                   clear                   parvar
148500130201     c                   movel     agbfbs        parvar
148600130204     c                   movel     fnlvv1ds      kpjbu
148700130206     c                   call      'FNLVV1R'
148800130204     c                   parm                    kpjba
148900130201     c                   parm                    parvar
149000130201     c                   endsr
149100000000     C**************************************************************************
149200000000     C* R O U T I N E    I N I Z I A L E
149300000000     C**************************************************************************
149400000000     C     *INZSR        BEGSR
149500950130     C*
149600000000     C     KART          KLIST
149700000000     C                   KFLD                    ARTAAS
149800000000     C                   KFLD                    ARTLNP
149900000000     C                   KFLD                    ARTNRS
150000000000     C                   KFLD                    ARTNSP
150100950130     C     KBTT          KLIST
150200950130     C                   KFLD                    BTTAAS
150300950130     C                   KFLD                    BTTLNP
150400950130     C                   KFLD                    BTTNRS
150500950130     C                   KFLD                    BTTNSP
150600031127     C     KBTP          KLIST
150700031127     C                   KFLD                    BTPflp
150800031127     C                   KFLD                    BTpAAS
150900031127     C                   KFLD                    BTpLNP
151000031127     C                   KFLD                    BTpNRS
151100031127     C                   KFLD                    BTpNSP
151200950130     C     KBLT          KLIST
151300950130     C                   KFLD                    BLTAAS
151400950130     C                   KFLD                    BLTLNP
151500950130     C                   KFLD                    BLTNRS
151600950130     C                   KFLD                    BLTNSP
151700041104     C     KAGp          KLIST
151800041104     C                   KFLD                    AGpAAS
151900041104     C                   KFLD                    AGpLNP
152000041104     C                   KFLD                    AGpNRS
152100041104     C                   KFLD                    AGpNSP
152200041108     C     KAGV          KLIST
152300041108     C                   KFLD                    AGvAAS
152400041108     C                   KFLD                    AGvLNP
152500041108     C                   KFLD                    AGvNRS
152600041108     C                   KFLD                    AGvNSP
152700000000     C     KSAV          KLIST
152800070504     C                   KFLD                    agbAAS
152900070504     C                   KFLD                    agbLNP
153000070504     C                   KFLD                    agbNRS
153100070504     C                   KFLD                    agbNSP
153200030108
153300030108     c     kFidta        Klist
153400070504     c                   Kfld                    agbAas
153500070504     c                   Kfld                    agbLnp
153600070504     c                   Kfld                    agbNrs
153700070504     c                   Kfld                    agbNsp
153800030108     c                   Kfld                    SavTrd
153900030108     c                   Kfld                    SavPoc
154000091021     C     Kblp46        KLIST
154100091021     C                   KFLD                    BlPtfp
154200091021     C                   KFLD                    BlpAAS
154300091021     C                   KFLD                    BlpLNP
154400091021     C                   KFLD                    BlpNRS
154500091021     C                   KFLD                    BlpNSP
154600150605     C     Kbrv07        KLIST
154700150605     C                   KFLD                    Blpfls
154800150605     C                   KFLD                    Blplna
154900150605     C                   KFLD                    Blpnrs
155000150605     C                   KFLD                    wncd
155100030320
155200030320     c     kTab          Klist
155300030320     c                   Kfld                    Codut
155400030320     c                   Kfld                    Cod
155500030320     c                   Kfld                    Key
155600000000     C*
155700950926     C     *LIKE         DEFINE    BLPDSE        WDSE
155800950926     C     *LIKE         DEFINE    ARBDET        WDET
155900950130     C     *LIKE         DEFINE    ARBDUT        WDUT
156000000000     C     *LIKE         DEFINE    ARTDAM        WDAM
156100950328     C     *LIKE         DEFINE    ARTDAM        WDAU
156200950130     C     *LIKE         DEFINE    ARBDPC        WDPC
156300950130     C     *LIKE         DEFINE    ARBDUC        WDUC
156400950130     C     *LIKE         DEFINE    ARBFAN        WFAN
156500000000     C*
156600000000     C                   ENDSR
