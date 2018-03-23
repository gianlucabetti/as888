000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200931130      *****************************************************************
000300021213      *  Nome programma:  FNLSC1R
000400941012      *  Descrizione   :  Stampa statistica partenze: partito
000500931130      *                   Programma esecuzione (stampa)
000600931130      *  Data creazione:  OTT 1993
000700931130      *****************************************************************
000800941012      *  Il pgm puo' essere lanciato dal filtro FNLSC0R oppure tramite
000900931130      *  le funzioni giornaliere, insieme ad altre stampe (guidate dalla
001000931130      *  tabella DS3L)
001100931130      *****************************************************************
001200950210      *  ??PASSAGGIO PARAMETRI:?
001300950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001400950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001500970207      *  DS1FPG   TIPO STASTISTICA
001600950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001700950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001800950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001900950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
002000950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002100950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002200950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002300950210      *****************************************************************
002400020813     ***FFNFGV04L  IF   E           K DISK
002500020813     FFNFGV06L  IF   E           K DISK
002600070112     FFNBRV09L  IF   E           K DISK
002700970128     FFNSPA01L  UF A E           K DISK
002800930930     FTABEL00F  IF   E           K DISK
002900090304     FFIART38L  IF   E           K DISK
003000950111     FFNBLP01L  IF   E           K DISK
003100950111     F                                     RENAME(FNBLP000:FNBLP001)
003200970129     FFNBLP06L  IF   E           K DISK
003300070523     FFNBLP05L  IF   E           K DISK
003400070523     F                                     RENAME(FNBLP000:FNBLP005)
003500020823     FFNBTT38L  IF   E           K DISK
003600020823     FFNBTP11L  IF   E           K DISK
003700020823     FFNBTP15L  IF   E           K DISK
003800020823     F                                     RENAME(FNBTP000:FNBTP015)
003900990518     FFNANM04L  IF   E           K DISK
004000930924     FAZORG01L  IF   E           K DISK
004100970204     FAZCLN01L  IF   E           K DISK
004200021016     fTntbe01l  uf   e           k Disk
004300090415     Ffista01L  uF   E           K DISK
004400930927     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
004500930927     F                                     USROPN
004600931130      *--TAB.INTERNE E SCHIERE---------------------------------------*
004700950209     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
004800970306     D SK              S             66    DIM(18) CTDATA PERRCD(1)
004900941012     D SK1             S             66    DIM(7) CTDATA PERRCD(1)
005000970204     D GME             S              1    DIM(31)
005100931130      *
005200000831     D L1              S              3  0 DIM(30)
005300020418     D L6              S              3  0 DIM(30)
005400950213     D LV2             S              3  0 DIM(30)
005500000615     D LIN             S              3  0 DIM(30)
005600000615     D SVLIN           S              3  0 DIM(30)
005700950111     D E3A             S              2    DIM(60)
005800970505     D D3A             S             75    DIM(60)
005900931130      *--SCHIERE FILIALE E AREA PER CALCOLI
006000941012      * N.B.: le schiere di 6 elementi sono cosi' suddivise:
006100941012      *       .  i primi tre (1-3) elementi sono i c/servizi dell'area
006200941012      *       .  i secondi tre (4-6)sono i totali di filiale
006300941012      *       .  i terzi tre (7-9) sono i c/servizi della filiale
006400931130      *--COLLI
006500001201     D SKA             S              7  0 DIM(9)
006600001201     D AKA             S              7  0 DIM(3)
006700001201     D TKA             S              7  0 DIM(3)
006800001201     D XKA             S              7  0 DIM(3)
006900931130      *--PESO
007000941012     D SKD             S              8  1 DIM(9)
007100931006     D AKD             S              8  1 DIM(3)
007200931006     D TKD             S              8  1 DIM(3)
007300931007     D XKD             S              8  1 DIM(3)
007400941012     D SKE             S              8  1 DIM(9)
007500931006     D AKE             S              8  1 DIM(3)
007600931006     D TKE             S              8  1 DIM(3)
007700931007     D XKE             S              8  1 DIM(3)
007800941012     D SKF             S              8  1 DIM(9)
007900931006     D AKF             S              8  1 DIM(3)
008000931006     D TKF             S              8  1 DIM(3)
008100931007     D XKF             S              8  1 DIM(3)
008200931130      *--VOLUME
008300970129     D SKI             S              9  4 DIM(9)
008400941005     D AKI             S              9  4 DIM(3)
008500941005     D TKI             S              9  4 DIM(3)
008600941005     D XKI             S              9  4 DIM(3)
008700931130      *--SPEDIZIONI
008800970129     D SKL             S              7  0 DIM(9)
008900931006     D AKL             S              7  0 DIM(3)
009000931006     D TKL             S              7  0 DIM(3)
009100931007     D XKL             S              7  0 DIM(3)
009200970129     D SKM             S              7  0 DIM(9)
009300970203     D AKM             S              7  0 DIM(3)
009400970203     D SKN             S              7  0 DIM(9)
009500970203     D AKN             S              7  0 DIM(3)
009600970206     D SKO             S              7  0 DIM(9)
009700970206     D SKP             S              7  0 DIM(9)
009800020418
009900020418     d sav_t06         S              3  0 dim(30)
010000020418
010100020418     d wc              s              3  0
010200021016
010300021016     d kTbeCod         s                   Like(TbeCod)
010400021016     d kTbeKe1         s                   Like(TbeKe1)
010500050829     d klin            s                   Like(ds1lnp)
010600050829     d kdre            s                   Like(spadre)
010700020418
010800931130      *--DS*---------------------------------------------------------*
010900931130      *
011000931130      * DS PER TESTARE QUANDO CAMBIA BOLLA (SI VALORIZZA LEGGENDO I SEGNACOLLI).
011100930930     D                 DS
011200090304     D  ARTAAS                 1      4  0
011300090304     D  ARTLNP                 5      7  0
011400090304     D  ARTNRS                 8      9  0
011500090304     D  ARTNSP                10     16  0
011600950111     D  DSTXT                  1     16  0
011700950111     D                 DS
011800950111     D  BTTAAS                 1      4  0
011900950111     D  BTTLNP                 5      7  0
012000950111     D  BTTNRS                 8      9  0
012100950111     D  BTTNSP                10     16  0
012200950111     D  DSTXT1                 1     16  0
012300931130      *
012400931130      * DS PER TESTARE SE BOLLETTATO IN PERIODO O IN PERIODO PRECEDENTE.
012500930930     D                 DS
012600941012     D  BLPAAS                 1      4  0
012700941012     D  BLPMGS                 5      8  0
012800941012     D  BLPDS                  1      8  0
012900040921      *
013000040921     D WLBDAT          DS                  INZ
013100040921     D  G02DAT                 1      8  0
013200040921     D  G02INV                 9     16  0
013300040921     D  G02ERR                17     17
013400040921     D  G02TGI                18     22  0
013500921222     D DS3A          E DS
013600950111     D* N.B.: SE VARIA LA LUNGHEZZA DI DS3A, MODIF.LA SCHIERA D3A
013700930927     D DS3I          E DS
013800921222     D DS3L          E DS
013900040921     D*DS5C          E DS
014000970707     D DS5P          E DS
014100950111     D DSQT1         E DS
014200000831     D OG143         E DS
014300950210     D DSLS01        E DS
014400970204     D  ANNODA                41     44  0
014500970204     D  MESEDA                45     46  0
014600970204     D  $GI                   47     48  0
014700920930     D CNCR80        E DS
014800920930     D KPJBA         E DS
014900920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015000950209     D* DS PER TRUL06R - CARICAMENTO £X
015100950209     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015200950314     D  T06                    1     90  0
015300950314     D                                     DIM(30)
015400970129      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015500970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
015600000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
015700000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
015800021016      * Ds date statistiche
015900021016     d dSdf          e ds
016000070523     D* DS PER LETTURA TABELLA "VPO" record DECO
016100070523     D Dvpodeco      E DS
016200020813      *
016300020813      * DEFINIZIONE CAMPI
016400020813      *
016500020813     D SAVRIS          S                   LIKE(DS1RIS) inz
016600020813     D WCAA            S                   LIKE(ANMCAA) inz
016700021220     D WVOLUM          S                   LIKE(VOLUM)  inz
016800021220     D WPESCOL         S                   LIKE(PESCOL) inz
016900020813     D KDUC            S                   LIKE(BLPDUC) inz
017000020813     D KDBR            S                   LIKE(BLPDBR) inz
017100070523     D KDrt            S                   LIKE(BLPDrt) inz
017200020813     D $TFA            S                   LIKE(BLPTFA) inz
017300020813     D W0010           S                   LIKE(SPASFC) inz
017400020813     D W0020           S                   LIKE(SPASFC) inz
017500020813     D W0030           S                   LIKE(SPASFC) inz
017600020813     D KNPG            S                   LIKE(BRVNPG) inz
017700020813     D SAVLNP          S                   LIKE(BLPLNP) inz
017800020813     D*
017900020813     D KCOD            S                   LIKE(TBLCOD) inz
018000020813     D KKEY            S                   LIKE(TBLKEY) inz
018100020813     D COMFIL          S                   LIKE(ORGFIL) inz
018200020813     D FLDTXT          S                   LIKE(DSTXT)  inz
018300090415     D ktrc            S                   LIKE(statrc) inz('P')
018400090415     D kfil            S                   LIKE(stafil) inz(000)
018500021213      *
018600021213     d Volum           s             12  4 inz
018700021213     d PesCol          s              8  1 inz
018800021213     d PesTax          s              8  0 inz
018900021213     d PesTas          s              9  1 inz
019000030213     d*** WsiVOL          s              1    inz
019100030213     d*** WsiPES          s              1    inz
019200021213     d Wvolu           s                   like(BLPvlf) inz
019300021213     d Wpeso           s                   like(BLPpkf) inz
019400931130      *---------------------------------------------------------------*
019500931130      *    RIEPILOGO INDICATORI
019600931130      *---------------------------------------------------------------*
019700931130      * 03    - IMMESSA DATA AL
019800970129      * 05    - ON,CLIENTE DISK B
019900931130      * 04    - ON,SE BOLLA=C/SERVIZI; OFF SE BOLLA DI FILIALE.
020000931130      * 06    - ON,SE BOLLA FINO A 35 Q. OFF SE OLTRE 35 Q.
020100931130      * 07    - ON,SE BOLLA BOLLETTATA IN PERIODO PRECEDENTE.(PER CICLO SPEDIZ.)
020200931130      * 30/32 - DI COMODO
020300940328      * 36    - ON,SE NON SI VUOLE IL TABULATO STAMPATO
020400931130      *---------------------------------------------------------------*
020500000000     C     *ENTRY        PLIST
020600000000     C                   PARM                    KPJBA
020700950210     C                   MOVEL     KPJBU         DSLS01
020800950210      *
020900931130      *
021000931130      *---------------------------------------------------------------*
021100931130      * LETTURA FILE SEGNACOLLI (COLLI).
021200090304     C     KART30        KLIST
021300930924     C                   KFLD                    COMFIL
021400950210     C                   KFLD                    DS1ADA
021500970129     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
021600970129     C     KBLP6         KLIST
021700970129     C                   KFLD                    COMFIL
021800970129     C                   KFLD                    KDUC
021900970204     C     KBLP6A        KLIST
022000970204     C                   KFLD                    COMFIL
022100970204     C                   KFLD                    KDUC
022200970204     C                   KFLD                    KDBR
022300970129     C     KBLP6B        KLIST
022400970129     C                   KFLD                    COMFIL
022500970129     C                   KFLD                    BLPDUC
022600970204     C     KBLP6C        KLIST
022700970204     C                   KFLD                    COMFIL
022800970204     C                   KFLD                    BLPDUC
022900970204     C                   KFLD                    BLPDBR
023000070523     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
023100070523     C     KBLP5         KLIST
023200070523     C                   KFLD                    COMFIL
023300070523     C                   KFLD                    Kduc
023400070523     C     KBLP5A        KLIST
023500070523     C                   KFLD                    COMFIL
023600070523     C                   KFLD                    Kduc
023700070523     C                   KFLD                    KDrt
023800070523     C     KBLP5B        KLIST
023900070523     C                   KFLD                    COMFIL
024000070523     C                   KFLD                    BLPDUC
024100931130      * LETTURA FILE BOLLE. (PER RISALIRE DAL COLLO ALLA BOLLA)
024200950111     C     KBLP01        KLIST
024300090304     C                   KFLD                    ARTAAS
024400090304     C                   KFLD                    ARTLNP
024500090304     C                   KFLD                    ARTNRS
024600090304     C                   KFLD                    ARTNSP
024700020823     C     KBTP11        KLIST
024800020823     C                   KFLD                    BTTFLP
024900020823     C                   KFLD                    BTTAAS
025000950111     C                   KFLD                    BTTLNP
025100950111     C                   KFLD                    BTTNRS
025200950111     C                   KFLD                    BTTNSP
025300970203     C     FNSPAK        KLIST
025400970128     C                   KFLD                    SIMFEL
025500000614     C                   KFLD                    LIN(XX)
025600970128     C                   KFLD                    DS1ADA
025700970204     C*
025800970204     C     KCLN          KLIST
025900970204     C                   KFLD                    SIMFEL
026000970204     C                   KFLD                    $TFA
026100970204     C                   KFLD                    ANNODA
026200970204     C                   KFLD                    MESEDA
026300970207     C*
026400970207     C     KANM          KLIST
026500970207     C                   KFLD                    WCAA
026600990518     C                   KFLD                    DS1ADA
026700000204     C     KBRV          KLIST
026800970505     C                   KFLD                    KNPG
026900970505     C                   KFLD                    ANMFLS
027000970505     C                   KFLD                    ANMLNA
027100970505     C                   KFLD                    ANMNRS
027200970505     C                   KFLD                    ANMSCN
027300970505     C                   KFLD                    SIMFEL
027400020823     C     KDTA          KLIST
027500020823     C                   KFLD                    SIMFEL
027600020823     C                   KFLD                    DS1ADA
027700021016
027800021016     c     Ktbe          Klist
027900021016     c                   Kfld                    kTbeCod
028000021016     c                   Kfld                    kTbeKe1
028100050829     c     KSPA2         Klist
028200050829     c                   Kfld                    DS1FEL
028300050829     c                   Kfld                    KLIn
028400050829     c     KSPA          Klist
028500050829     c                   Kfld                    DS1FEL
028600050829     c                   Kfld                    KLIn
028700050829     c                   Kfld                    Kdre
028800090415     c     KSTA          Klist
028900090415     c                   Kfld                    Ktrc
029000090415     c                   Kfld                    DS1FEL
029100090415     c                   Kfld                    Kfil
029200090415     c                   Kfld                    ds1ada
029300931130      *---------------------------------------------------------------*
029400931130      * ESEGUO OPERAZ. DA FARE UNA VOLTA SOLA, ALL'INIZIO
029500930927     C                   EXSR      INIZIO
029600950209      *
029700950209      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
029800950209     C                   EXSR      COMUNE
029900970129     C*
030000970129     C* CALL PGM PER CHIUDERE FILES
030100970129     C     DS1RIS        IFNE      'R'
030200970129     C                   MOVEL     'C'           D98TLA
030300970925     C                   CALL      'TNSD99R'
030400970925     C                   PARM                    DS99
030500970203     C                   END
030600931130      *
030700920930     C                   SETON                                        LR
030800950209      *
030900020212     C*****************************************************************
031000020212     C*  PARTE COMUNE STAMPE GIORNALIERE
031100020212     C*
031200020212     C*  . REPERIMENTO 'SIMFEL'
031300020212     C*  . CARICAMENTO '£1'
031400020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
031500020212     C*  . CODA DI STAMPA ('£OUTQ')
031600020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
031700020212     C*  . MODULO PER LA STAMPA ('£MOD')
031800020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
031900020212     C*****************************************************************
032000020212     C     COMUNE        BEGSR
032100020212     C*
032200020212     C*****************************************************************
032300020212     C*       RIEPILOGO INDICATORI:
032400020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
032500020212     C* 11    -           STAMPA PER 1L
032600020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
032700020212     C*****************************************************************
032800020813     ***C                   Z-ADD     1             CODUT
032900020212     C*
033000020212     C* Reperimento parametri aziendali
033100020212     C                   CALL      'X§PARUT'
033200020212     C                   PARM                    UT§DSE
033300020212     C                   MOVEL     RAGUT         RSUT             20
033400020212     C                   MOVEL     REC80         CNCR80
033500020212     C*
033600020212     C* Chiave di accesso per il file tabelle
033700020212     C     KTAB1         KLIST
033800020212     C                   KFLD                    CODUT
033900020212     C                   KFLD                    KCOD
034000020212     C                   KFLD                    KKEY
034100020212     C     KTAB2         KLIST
034200020212     C                   KFLD                    CODUT
034300020212     C                   KFLD                    KCOD
034400020212     C*
034500020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
034600020212     C*
034700020212     C* DETERMINO DATA E ORA DI ESECUZIONE
034800020212     C                   TIME                    WTIME            14 0
034900020212     C                   MOVE      WTIME         WDATE             8 0
035000020212     C                   MOVEL     WTIME         UTIME             6 0
035100040921     c*
035200040921     C                   CLEAR                   WLBDAT
035300040921     C                   MOVE      Wtime         G02DAT
035400040921     C                   CALL      'XSRDA8'
035500040921     C                   PARM                    WLBDAT
035600040921     C                   Z-ADD     G02INV        dateu             8 0
035700020212     C*
035800020418     C* CARICO TABELLA '£1'
035900020212     C                   CLEAR                   KPJBU
036000020212     C                   CLEAR                   DSUL06
036100020212     C                   MOVE      '£1'          D06COD
036200020212     C                   MOVEL     SIMFEL        D06KEY
036300050802     C                   MOVE      'S'           D06KEY
036400050802     C                   MOVEL     ds1ada        D06drf
036500020212     C                   MOVEL     DSUL06        KPJBU
036600020212     C                   CALL      'TRUL06R'
036700020212     C                   PARM                    KPJBA
036800020212     C                   MOVEL     KPJBU         DSUL06
036900020418     c                   movea     t06           sav_t06
037000020418
037100020418      * se in arrivo carico la £6
037200020418 b1  c                   if        ds1tbo = 'A'
037300020418     c                   clear                   wc
037400020418 b2  c                   do        30            wa
037500020422     c                   if        sav_t06(wa) = *zeros
037600020422     c                   leave
037700020422     c                   endif
037800020418     c                   clear                   kpjbu
037900020418     c                   clear                   dsul06
038000050802     C                   MOVEL     ds1ada        D06drf
038100020418     c                   eval      d06cod = '£6'
038200020418     c                   movel     sav_t06(wa)   d06key
038300050802     C                   MOVE      'S'           D06KEY
038400020418     c                   eval      d06esc = 'S'
038500020419     c                   movel     dsul06        kpjbu
038600020418     c                   call      'TRUL06R'
038700020418     c                   parm                    kpjba
038800020418     c                   movel     kpjbu         dsul06
038900020418 b3  c                   do        30            wb
039000020418     c                   if        t06(wb) = *zeros
039100020418     c                   leave
039200020418     c                   endif
039300020418     c                   add       1             wc
039400020418     c                   z-add     t06(wb)       l6(wc)
039500020418 e3  c                   enddo
039600020418 e2  c                   enddo
039700020212     C*
039800020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
039900020212     C                   Z-ADD     *ZERO         WB
040000020418 b2  C                   DO        29            WA
040100020418     C     sav_T06(WA)   LOOKUP    L6                                     30
040200020212     C  N30              ADD       1             WB
040300020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
040400020418 e2  C                   ENDDO
040500020418 x1  C                   ELSE
040600020418     C                   MOVEA     sav_T06       L1
040700020418 e1  C                   ENDIF
040800020212     C*
040900070103     C* ESCLUDO DA £1 LINEE NON consentite
041000020212     C                   MOVEA     L1            LIN
041100020212     C                   CLEAR                   L1
041200020212     C                   Z-ADD     *ZERO         WB                3 0
041300020212     C     1             DO        30            WPT               3 0
041400020212     C     LIN(WPT)      IFNE      0
041500020212     C                   CLEAR                   FNLSC4
041600020212     C                   Z-ADD     LIN(WPT)      IC4LIN
041700020212     C                   MOVE      'B'           IC4FLG
041800020212     C                   MOVEL     FNLSC4        KPJBU
041900020212     C                   CALL      'FNLSC4R1'
042000020212     C                   PARM                    KPJBA
042100050802     C                   PARM                    ds1ada
042200020212     C                   MOVEL     KPJBU         FNLSC4
042300020212     C     OC4ESE        IFEQ      'S'
042400020212     C                   ADD       1             WB
042500020212     C                   MOVE      LIN(WPT)      L1(WB)
042600020212     C                   ENDIF
042700020212     C                   ENDIF
042800020212     C                   ENDDO
042900070103     C*
043000070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
043100070103     C                   Z-ADD     *ZERO         WB                3 0
043200070103     C                   DO        29            WA                3 0
043300070103     C     L1(WA)        IFNE      *ZEROS
043400070103     C     L1(WA)        ANDNE     SIMFEL
043500070103     C                   ADD       1             WB
043600070103     C                   Z-ADD     L1(WA)        LV2(WB)
043700070103     C                   ENDIF
043800070103     C                   ENDDO
043900020212     C*
044000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
044100020212     C*
044200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
044300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
044400020212     C* la stampa e' lanciata da menu
044500020212     C                   MOVEL     '3L'          KCOD
044600020212     C                   MOVEL     DS1PGM        KKEY
044700020212     C                   MOVE      DS1FPG        KKEY
044800020212     C     KTAB1         CHAIN     TABEL00F                           30
044900020212     C  N30              MOVEL     TBLUNI        DS3L
045000020212     C   30              MOVEL     'F'           §3LD1L
045100020212     C   30              MOVEL     ' '           §3LD2L
045200020212     C   30              MOVEL     '*STD    '    §3LDST
045300020212     C*
045400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
045500020212     C                   MOVEL     §3LDST        £MOD             10
045600020212     C                   CLEAR                   LIN
045700020212     C*
045800020212     C* Verifico chi ha lanciato la stampa
045900020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
046000020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
046100020212     C* DS1FGS<>0  --->  si proviene da menu
046200020212     C     DS1FGS        IFEQ      0
046300020212     C*
046400020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
046500020212     C* lancio, voglio la stampa;  invece
046600020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
046700020212     C*
046800020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
046900020212     C* creare stampa per 1°lvl. (se stessa)
047000020212     C     DS1LNP        IFEQ      SIMFEL
047100020212     C*
047200020212     C* Il flag §3LD1L ha i seguenti significati:
047300020212     C*    ' ' = tutti i dati
047400020212     C*    '1' = solo filiale di 1' livello
047500020212     C     §3LD1L        IFEQ      '1'
047600020212     C                   SETON                                        11
047700020212     C                   END
047800020212     C*
047900020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
048000020212     C* nella stampa
048100020212     C*
048200020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
048300020212     C  N11              MOVEA     L1            LIN
048400020212     C   11              MOVEA     SIMFEL        LIN(1)
048500020212     C*
048600020212     C* IMPOSTO LA STAMPANTE:
048700020212     C* STAMPANTE TABULATI
048800020212     C                   MOVEL     '5P'          KCOD
048900020212     C                   MOVEL     'T'           W004A             4
049000020212     C                   MOVE      LIN(1)        W004A
049100020212     C                   MOVEL(P)  W004A         KKEY
049200020212     C     KTAB1         CHAIN     TABEL00F                           99
049300020212     C     *IN99         IFEQ      *OFF
049400020212     C                   MOVEL     TBLUNI        DS5P
049500020212     C                   MOVEL     §5POTQ        £OUTQ            10
049600020212     C                   END
049700020212     C*
049800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
049900020212     C                   EXSR      LEGGI
050000020212     C*
050100020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
050200020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
050300020212     C                   ELSE
050400020212     C*
050500020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
050600020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
050700020212     C*
050800020212     C* Il significato del fla §3LD2L e' il seguente:
050900020212     C*    ' ' = tutti i dati
051000020212     C*    'F' = per ogni 2' livello
051100020212     C     §3LD2L        IFEQ      'F'
051200020212     C                   SETON                                        14
051300020212     C                   ENDIF
051400020212     C*
051500020212     C                   Z-ADD     1             WB
051600020212     C                   MOVEA     *ZEROS        LIN
051700020212     C  N14              MOVEA     L1            LIN
051800020212     C*
051900020212     C* Lancio per tutti i 2° lvl
052000020212     C     DS1LNP        IFEQ      0
052100020212     C*
052200020212     C* LV2 contiene tutte le filiali di 2° lvl
052300020212     C     LV2(WB)       DOWNE     0
052400020212      *
052500020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
052600020212     C                   CLEAR                   FNLSC4
052700020212     C                   Z-ADD     LV2(WB)       IC4LIN
052800020212     C                   MOVEL     FNLSC4        KPJBU
052900020212     C                   CALL      'FNLSC4R1'
053000020212     C                   PARM                    KPJBA
053100050802     C                   PARM                    ds1ada
053200050802     C                   MOVEL     KPJBU         FNLSC4
053300020212      *
053400020212     C     OC4PT1        IFEQ      'S'
053500020212     C     §3LPT         ANDEQ     'N'
053600020212     C                   GOTO      NOLEGG
053700020212     C                   ENDIF
053800020212      *
053900020212     C     OC4EXP        IFEQ      'S'
054000020212     C     §3LEXP        ANDEQ     'N'
054100020212     C                   GOTO      NOLEGG
054200020212     C                   ENDIF
054300020212      *
054400020212     C     OC4DPD        IFEQ      'S'
054500020212     C     §3LDPD        ANDEQ     'N'
054600020212     C                   GOTO      NOLEGG
054700020212     C                   ENDIF
054800020212      *
054900020212     C     OC4BAR        IFEQ      'S'
055000020212     C     §3LBAR        ANDEQ     'N'
055100020212     C                   GOTO      NOLEGG
055200020212     C                   ENDIF
055300020212      *
055400020212     C     OC4FED        IFEQ      'S'
055500020212     C     §3LFED        ANDEQ     'N'
055600020212     C                   GOTO      NOLEGG
055700020212     C                   ENDIF
055800020212      *
055900020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
056000020212     C*  dati degli altri 2° lvl
056100020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
056200020212     C   14              MOVEL     LV2(WB)       LIN(1)
056300020212     C*
056400020212     C* IMPOSTO LA STAMPANTE
056500020212     C* STAMPANTE TABULATI
056600020212     C                   MOVEL     '5P'          KCOD
056700020212     C                   MOVEL     'T'           W004A             4
056800020212     C                   MOVE      LV2(WB)       W004A
056900020212     C                   MOVEL(P)  W004A         KKEY
057000020212     C     KTAB1         CHAIN     TABEL00F                           99
057100020212     C     *IN99         IFEQ      *OFF
057200020212     C                   MOVEL     TBLUNI        DS5P
057300020212     C                   MOVEL     §5POTQ        £OUTQ            10
057400020212     C                   END
057500020212     C*
057600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
057700020212     C                   EXSR      LEGGI
057800020212     C*
057900020212     C     NOLEGG        TAG
058000020212     C*
058100020212     C                   ADD       1             WB
058200020212     C                   ENDDO
058300020212     C*
058400020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
058500020212     C                   ELSE
058600020212     C*
058700020212     C* Lancio solo per la 2° lvl richiesta
058800020212     C   14              MOVEL     DS1LNP        LIN(1)
058900020212     C*
059000020212     C* STAMPANTE
059100020212     C* STAMPANTE TABULATI
059200020212     C                   MOVEL     '5P'          KCOD
059300020212     C                   MOVEL     'T'           W004A             4
059400020212     C                   MOVE      DS1LNP        W004A
059500020212     C                   MOVEL(P)  W004A         KKEY
059600020212     C     KTAB1         CHAIN     TABEL00F                           99
059700020212     C     *IN99         IFEQ      *OFF
059800020212     C                   MOVEL     TBLUNI        DS5P
059900020212     C                   MOVEL     §5POTQ        £OUTQ            10
060000020212     C                   END
060100020212     C*
060200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
060300020212     C                   EXSR      LEGGI
060400020212     C*
060500020212     C                   ENDIF
060600020212     C                   ENDIF
060700020212     C*
060800020212     C                   ELSE
060900020212     C*
061000020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
061100020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
061200020212     C     DS1LNP        IFEQ      0
061300020212     C                   MOVEA     L1            LIN
061400020212     C* ...altrimenti elaboro solo se stessa
061500020212     C                   ELSE
061600020212     C                   MOVEL     DS1LNP        LIN(1)
061700020212     C                   ENDIF
061800020212     C*
061900020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
062000020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
062100020212     C* PRENDO QUELLA PRESENTE IN KPJBA
062200020212     C     DS1FGS        IFEQ      SIMFEL
062300020212     C                   MOVEL     KQOTD         £OUTQ
062400030627     C**!!!              MOVEL     KQOTD         £OUTQF
062500020212     C                   ELSE
062600020212     C* STAMPANTE TABULATI
062700020212     C                   MOVEL     '5P'          KCOD
062800020212     C                   MOVEL     'T'           W004A             4
062900020212     C                   MOVE      DS1FGS        W004A
063000020212     C                   MOVEL(P)  W004A         KKEY
063100020212     C     KTAB1         CHAIN     TABEL00F                           99
063200020212     C     *IN99         IFEQ      *OFF
063300020212     C                   MOVEL     TBLUNI        DS5P
063400020212     C                   MOVEL     §5POTQ        £OUTQ            10
063500020212     C                   END
063600020212     C                   ENDIF
063700020212     C*
063800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
063900020212     C                   EXSR      LEGGI
064000020212     C                   ENDIF
064100020212     C*
064200020212     C                   ENDSR
064300931130      *****************************************************************
064400931130      * SUBROUTINE INIZIO: OPERAZIONI DA FARE UNA VOLTA SOLA ALL'INIZIO
064500931130      ****************************************************************
064600930927     C     INIZIO        BEGSR
064700950209      *
064800950209     C                   Z-ADD     1             CODUT
064900940104      *
065000941012      *  Stampa o ristampa ?
065100941012     C                   MOVE      'RISTAMPA'    STRIP             8
065200950404     C     DS1RIS        IFEQ      'S'
065300941012     C                   MOVE      'STAMPA  '    STRIP
065400941012     C                   ENDIF
065500941012      *
065600941012      * Scrivo, in stampa, se selezionata filiale o area
065700941012     C                   MOVE      *BLANKS       STRIP1            8
065800950210     C     DS1LNP        IFEQ      *ZERO
065900941012     C                   MOVEL     'AREA'        STRIP1
066000950210     C                   MOVE      DS1FEL        STRIP1
066100941012     C                   ELSE
066200960925     C                   MOVEL     'P.O.'        STRIP1
066300950210     C                   MOVE      DS1LNP        STRIP1
066400941012     C                   ENDIF
066500940429      *
066600941012      * Per sicurezza, se la data finale e' nulla metto quella iniziale
066700950210     C     DS1AAL        IFEQ      *ZERO
066800950210     C                   Z-ADD     DS1ADA        DS1AAL
066900970130     C                   Z-ADD     DS1GDA        DS1GAL
067000941012     C                   ENDIF
067100931130      *
067200931130      * TESTO SE E' STATA IMMESSA 'DATA A' ED E' <> DA 'DATA DA', SETON 03.
067300950210     C     DS1AAL        IFNE      0
067400950210     C     DS1AAL        ANDNE     DS1ADA
067500930927     C                   SETON                                        03
067600930927     C                   ENDIF
067700940509      *
067800941012      * Accendo il 36 se non desidero la stampa
067900950210     C     DS1RIS        COMP      *BLANKS                                36
068000940509      *
068100950111      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
068200950209     C                   MOVEL     'QT'          KCOD
068300950209     C                   MOVEL     '1       '    KKEY
068400950209     C     KTAB1         CHAIN     TABEL00F                           32
068500950111     C  N32              MOVEL     TBLUNI        DSQT1
068600950111     C   32              CLEAR                   DSQT1
068700931130      *
068800931130      * REPERIMENTO TABELLA VARIABILI PGM
068900950209     C                   MOVEL     '3I'          KCOD
069000950209     C                   MOVEL     '1       '    KKEY
069100950209     C     KTAB1         CHAIN     TABEL00F                           32
069200930927     C  N32              MOVEL     TBLUNI        DS3I
069300930927     C   32              Z-ADD     20            §3IARR
069400950111      *
069500950111      * METTO IN SCHIERA LA TABELLA 3A PER EVITARE CHAIN SUCCESSIVE
069600950111     C                   Z-ADD     *ZERO         XX
069700950209     C                   MOVE      '3A'          KCOD
069800950209     C     KTAB2         SETLL     TABEL00F
069900950209     C     KTAB2         READE     TABEL00F                               32
070000950111     C     *IN32         DOWEQ     *OFF
070100950111     C                   ADD       1             XX
070200950209     C                   MOVEL     TBLKEY        E3A(XX)
070300950111     C                   MOVEL     TBLUNI        D3A(XX)
070400950209     C     KTAB2         READE     TABEL00F                               32
070500950111     C                   ENDDO
070600070523     c
070700070523     c* Carico la tabella VPO per vedere da quando cambia il calcolo
070800070523     c*  della affidabilità partenza
070900070523     C                   MOVEL     'VPO'         kTBECOD
071000070523     C                   MOVEL(P)  'DECO'        kTBEKE1
071100070523     C     KTBE          CHAIN(N)  TNTBE01L                           21
071200070523     C  N21              MOVEL     TBEUNI        DVPODECO
071300070523     C*
071400930927     C                   ENDSR
071500930927     C*****************************************************************
071600930924     C* SUBROUTINE LEGGI: CICLO PRINCIPALE
071700930924     C*****************************************************************
071800920930     C     LEGGI         BEGSR
071900000615     C                   CLEAR                   SAVRIS
072000941012      *
072100941012      * Se sono nelle stampe giornaliere non voglio diciture
072200950210     C     DS1FGS        IFEQ      *ZERO
072300941012     C     SIMFEL        ANDNE     *ZERO
072400941012     C                   MOVE      *BLANKS       STRIP
072500941012     C                   MOVE      *BLANKS       STRIP1
072600941012     C                   ENDIF
072700941012      *
072800970129      * Cancello dal file statistiche i record della giornata che vado
072900000614      * a elabor. ELABORO SEMPRE TUTATA LA L1 E STAMPO LIN
073000000614      * D'ORA IN AVANTI IL PGM SARA' SEMRPE CHIAMATO SEPARATAMENTE
073100000614      * PERELABORARE E PER STAMPARE
073200950210     C     DS1RIS        IFNE      'R'
073300000614     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
073400000614     C                   CLEAR                   KPJBU
073500000614     C                   CLEAR                   DSUL06
073600000614     C                   MOVE      '£1'          D06COD
073700000614     C                   MOVEL     SIMFEL        D06KEY
073800050802     C                   MOVE      'S'           D06KEY
073900050802     C                   MOVEL     ds1ada        D06drf
074000000614     C                   MOVEL     DSUL06        KPJBU
074100000614     C                   CALL      'TRUL06R'
074200000614     C                   PARM                    KPJBA
074300000614     C                   MOVEL     KPJBU         DSUL06
074400000615     C**
074500000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
074600000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
074700000615     C*  CHE USO ALLA FINE
074800000615     C     DS1RIS        IFNE      ' '
074900000615     C                   MOVEA     LIN           SVLIN
075000000615     C                   MOVEL     DS1RIS        SAVRIS
075100000615     C                   CLEAR                   DS1RIS
075200000615     C                   ENDIF
075300000615     C*
075400000615     C                   MOVEA     T06           LIN
075500941012      *
075600941012     C                   Z-ADD     1             XX                3 0
075700000614     C     LIN(XX)       DOWGT     *ZERO
075800970129      *
075900970129     C     FNSPAK        SETLL     FNSPA01L
076000970129     C     FNSPAK        READE     FNSPA01L                               99
076100940509     C*
076200940509     C     *IN99         DOWEQ     *OFF
076300970128     C                   DELETE    FNSPA000
076400970128     C     FNSPAK        READE     FNSPA01L                               99
076500941012     C                   ENDDO
076600940509      *
076700941012     C                   ADD       1             XX
076800941012     C                   ENDDO
076900941012     C                   ENDIF
077000950209      *
077100950209      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
077200950209     C                   MOVEA     £MOD          CMD(33)
077300950209     C                   MOVEA     £OUTQ         CMD(51)
077400931130      *
077500931130      * SE NON VA BENE L'OVERRIDE, IL PGM VA A FINE.
077600950209     C                   Z-ADD     82            LUNG             15 5
077700950209     C                   MOVEL     *BLANKS       COMMAN           82
077800931130     C                   MOVEA     CMD           COMMAN
077900950209     C                   CALL      'QCMDEXC'                            99
078000920930     C                   PARM                    COMMAN
078100920930     C                   PARM                    LUNG
078200940509     C   99              GOTO      ENDLEG
078300931130      *
078400020813     C* CONTATORE 'XD': USATO PER IL CONTEGGIO FILIALI PER SALTO PAGINA.
078500020813     C                   Z-ADD     1             XC                3 0
078600020813     C                   Z-ADD     0             XD                3 0
078700941012     C  N36              OPEN      PRTF198
078800931013     C****STAMPA TESTATA
078900941012     C  N36              EXCEPT    TESTA
079000970226     C                   SETOFF                                       OF
079100000615     C*
079200000615     C* SE CLEARATO DS1RIS ACCENDO INDICATORE
079300000615     C     DS1RIS        IFEQ      ' '
079400000615     C                   SETON                                        36
079500000615     C                   ENDIF
079600940509     C* SE IN LIN,1 C'E' IL 1°LVL, (08) ELABORO I TRANSITI.
079700940509     C     SIMFEL        LOOKUP    LIN                                    08
079800950403     C*
079900970228     C                   CLEAR                   WDIS
080000970505     C                   CLEAR                   WDIS88
080100970228     C*
080200950403     C     *IN08         IFEQ      *ON
080300970228     C     DS1RIS        ANDNE     'R'
080400950403     C*
080500990517     C* SOMMO LE ANOMALIE DI TIPO "056-DISGUIDI" AI TRANSITI
080600990517     C                   Z-ADD     56            WCAA
080700990518     C     KANM          SETLL     FNANM000
080800990518     C     KANM          READE     FNANM000                               32
080900950403     C*
081000950403     C     *IN32         DOWEQ     *OFF
081100021023      * Deve elaborare solo le anomalie con ANMFLE della L1
081200021023     c     AnmFle        Lookup    L1                                     99
081300021023     c                   If        Not *In99
081400021023     c                   GoTo      Rileggi
081500021023     c                   EndIf
081600970305     C     ANMAIE        IFEQ      'E'
081700950403     C                   ADD       1             WDIS              6 0
081800970505     C* CERCO LA RELATIVA SPUNTA PARTENZA
081900970505     C                   Z-ADD     1             KNPG
082000070112     C     KBRV          CHAIN     FNBRV09L                           32
082100970505     C  N32BRVNPS        IFEQ      88
082200970505     C     BRVNPS        OREQ      89
082300970505     C     BRVNPS        OREQ      98
082400970505     C     BRVNPS        OREQ      96
082500970502     C                   ADD       1             WDIS88            6 0
082600970502     C                   ENDIF
082700970505     C                   ENDIF
082800950403     C*
082900021023     c     Rileggi       Tag
083000021023
083100990518     C     KANM          READE     FNANM000                               32
083200950403     C                   ENDDO
083300950403     C                   ENDIF
083400950403     C*
083500950403     C*
083600941012      * Se richiesta 'S'=Stampa elaboro gli archivi, altrimenti leggo il file
083700970129      * statistiche (FNSPA00F) e lo stampo direttamente
083800950210     C     DS1RIS        IFNE      'R'
083900970204     C* VERIFICO SU CALENDARIO SE GIORNO FESTIVO E LO MEMORIZZO
084000970204     C* (MI SERVE PER SAPERE IL TIPO RECORD DA MEMORIZZARE IN FNSPA)
084100970204     C                   CLEAR                   $TFA
084200970204     C                   CLEAR                   WFES
084300970204     C     KCLN          CHAIN     AZCLN01L                           32
084400970204     C     *IN32         IFEQ      *OFF
084500970204     C                   MOVEA     CLNMAT        GME
084600970204     C                   MOVE      GME($GI)      WFES              1
084700970204     C                   END
084800930924     C*****
084900930924     C*CICLO PER TUTTE LE FILIALI (LINEE DI PARTENZA)
085000020813     C     LIN(XC)       DOWNE     0
085100931011     C*
085200020813     C                   ADD       1             XD
085300930924     C*VALORIZZO IL CAMPO CHIAVE ...LINEA DI PARTENZA
085400020813     C                   MOVEL     LIN(XC)       COMFIL
085500930928     C* REPERIMENTO DECODIFICA LINEA PARTENZA
085600930928     C     COMFIL        CHAIN     AZORG                              32
085700970203     C     *IN32         IFEQ      *OFF
085800970203     C     ORGDE5        IFNE      *BLANKS
085900970203     C                   MOVEL     ORGDE5        DESLNP
086000970203     C                   ELSE
086100970203     C                   MOVEL     ORGDES        DESLNP           10
086200970203     C                   END
086300970203     C                   ELSE
086400970203     C                   MOVEL     *BLANKS       DESLNP
086500970203     C                   END
086600931011     C*
086700930924     C**ELABORAZIONE FILIALI (LNP) CON MEMORIZZAZIONE DEI TOTALI
086800000831     C                   EXSR      ELALNP
086900000831     C**ELABORAZIONE ANOMALIE 105 PER LA LINEA INTERESSATA
087000000831     C                   EXSR      ELA105
087100930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
087200930924     C                   EXSR      TOTLNP
087300020813     C                   ADD       1             XC
087400930924     C                   ENDDO
087500930924     C*****
087600930924     C**CALCOLO % STAMPA DEI C/SERVIZI (CONTO SERVIZI)
087700930924     C                   EXSR      TOTCS
087800931013     C**CALCOLO % E STAMPA AREA(TUTTE LE FILIALI + C/SERVIZI).
087900930924     C                   EXSR      TOTARE
088000931006     C** PER 08 ELABORO I TRANSITI
088100931006     C   08              EXSR      ELATRA
088200930924     C**CALCOLI % E STAMPA DEI TRANSITI
088300931006     C   08              EXSR      TOTTRA
088400940103      *
088500941012     C                   ELSE
088600970129      * Ristampo dal file FNSPA00F
088700941012     C                   EXSR      RISTAM
088800941012     C                   ENDIF
088900000615     C*
089000000615     C* SE DEVO ANCHE ELABORARE, LO FACCIO ORA
089100000615     C     SAVRIS        IFEQ      'S'
089200000615     C                   SETOFF                                       36
089300000615     C                   MOVEL     'R'           DS1RIS
089400000615     C                   MOVEA     SVLIN         LIN
089500000615     C                   EXSR      RISTAM
089600000615     C                   ENDIF
089700940103      *
089800930924     C**CALCOLI % E STAMPA DEI TOTALI GENERALI
089900941012      * SOLO SE STAMPATA ANCHE FILIALE DI 1° LIVELLO
090000000308      * OPPURE SE SONO IN RISTAMPA E HO STAMPATO LA RIGA DEI TRANSITI
090100000308     C     *IN08         IFEQ      *ON
090200000308     C     DS1RIS        OREQ      'R'
090300000308     C     TOTXKA        ANDGT     0
090400000308     C                   EXSR      TOTGEN
090500000308     C                   ENDIF
090600940405      *
090700941012      * Stampo la dicitura finale "N.B."
090800941012     C  N36              EXCEPT    FINSTA
090900931222      *
091000931222      *CHIUSURA FILE DI STAMPA
091100941012     C  N36              CLOSE     PRTF198
091200940223      *
091300941012      * Aggiorno la data ultima elaborazione
091400950210     C     DS1RIS        IFNE      'R'
091500021016
091600021016     c                   Eval      kTbeCod = 'SDF'
091700021016     c                   Movel(p)  Simfel        kTbeKe1
091800021016     c     Ktbe          Chain     Tntbe01l
091900021016     c                   If        %Found(Tntbe01l)
092000021016     c                   Movel     TbeUni        dSdf
092100021016     c                   If        §SdfParP = *Zeros or §SdfParP > Ds1Ada
092200021016     c                   Z-Add     Ds1Ada        §SdfParP
092300021016     c                   EndIf
092400021016     c                   If        §SdfParU < Ds1Ada
092500021016     c                   Z-Add     Ds1Ada        §SdfParU
092600021016     c                   EndIf
092700021016     c                   Movel     dSdf          TbeUni
092800021016     c                   Update    Tntbe000
092900021016     c                   EndIf
093000941012     C                   ENDIF
093100090415     c
093200090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
093300090415     c*  in FISTA
093400090415     c                   if        savris='S'
093500090415     c     ksta          delete    fista01l
093600090415     C                   ENDIF
093700090415      *
093800940223      *
093900940509     C     ENDLEG        ENDSR
094000930924     C*****************************************************************
094100930924     C* SUBROUTINE ELALNP: ELABORAZIONE FILIALI (LINEE DI PARTENZA)
094200930924     C*****************************************************************
094300930924     C     ELALNP        BEGSR
094400950111     C*
094500931008     C                   Z-ADD     0             FLDTXT
094600931013     C*ELABORO COLLI
094700090304     C     KART30        SETLL     FiART38L
094800930924     C*
094900090304     C     KART30        READE     FiART38L                               31
095000930924     C* CICLO LETTURA COLLI
095100970130    1C     *IN31         DOWEQ     *OFF
095200931013     C* VEDERE I DATI  BOLLA SOLO AL CAMBIO BOLLA
095300970130    2C     DSTXT         IFNE      FLDTXT
095400950111     C                   Z-ADD     DSTXT         FLDTXT
095500950111     C     KBLP01        CHAIN     FNBLP01L                           32
095600931130      *
095700930927     C*SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
095800970130    3C     *IN32         IFEQ      *OFF
095900950111     C                   Z-ADD     1             WA                3 0
096000950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
096100950111     C   32              MOVEL     D3A(WA)       DS3A
096200950111     C  N32              CLEAR                   DS3A
096300931130      *
096400970505     C* REPERIMENTO DATI BOLLA
096500931007     C                   EXSR      DTBOL
096600931007     C* REPERIMENTO DATI BOLLA, PER CALCOLO.
096700930927     C                   EXSR      DATBOL
096800931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN UN RECUPERO
096900970130   X3C                   ELSE
097000930924     C                   MOVEL     'R'           §3ARBL
097100970130    3C                   ENDIF
097200970130    2C                   ENDIF
097300930924     C*
097400931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
097500970130    2C     §3ARBL        IFNE      'R'
097600930924     C*ESECUZIONE SUBROUTINE DEI CALCOLI
097700930924     C                   EXSR      CALC
097800970130    2C                   ENDIF
097900930924     C*
098000090304     C     KART30        READE     FiART38L                               31
098100970130    1C                   ENDDO
098200070523     c
098300070523     c* Se data elaborazione <= data calcolo affidabilità partenza
098400070523     c*  con data ritiro, calcolo a modo vecchio
098500070523     c                   if        ds1ada<§vponpi
098600930927     C***
098700931013     C*ELABORO SPEDIZIONI
098800970129     C**
098900970129     C** I° CICLO --> DUC = 0 DBR <= DATA ELABORAZIONE
099000970204     C*               ESCLUDENDO QUELLE CON DATA BORDERO' = 0
099100970129     C**
099200970129     C                   MOVE      *ZEROS        KDUC
099300970204     C                   MOVE      *ZEROS        KDBR
099400970204     C     KBLP6A        SETGT     FNBLP06L
099500970129     C     KBLP6         READE     FNBLP06L                               31
099600970130    1C     *IN31         DOWEQ     *OFF
099700970203     C     BLPDBR        ANDLE     DS1ADA
099800931130      * TEST SE E' RECUPERO
099900950111     C                   Z-ADD     1             WA
100000950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
100100950111     C   32              MOVEL     D3A(WA)       DS3A
100200950111     C  N32              CLEAR                   DS3A
100300930927     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
100400970203    2C     §3ARBL        IFNE      'R'
100500970304     C     BLPNFV        ANDNE     99999
100600970304     C     BLPNFV        ANDNE     99998
100700990907     C     BLPCBO        ANDNE     '£ '
100800070525     c     blppdr        andgt     0
100900931007     C                   EXSR      CSPE
101000970203    2C                   ENDIF
101100970203     C     KBLP6         READE     FNBLP06L                               31
101200970130    1C                   ENDDO
101300970129     C**
101400970206     C** II° CICLO --> DUC = DATA ELABORAZIONE
101500970129     C**
101600970218     C                   Z-ADD     DS1ADA        KDUC
101700970206     C     KBLP6         SETLL     FNBLP06L
101800970206     C     KBLP6         READE     FNBLP06L                               31
101900970130    1C     *IN31         DOWEQ     *OFF
102000970129      * TEST SE E' RECUPERO
102100970129     C                   Z-ADD     1             WA
102200970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
102300970129     C   32              MOVEL     D3A(WA)       DS3A
102400970129     C  N32              CLEAR                   DS3A
102500970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
102600970130    2C     §3ARBL        IFNE      'R'
102700970129     C                   EXSR      CSPE
102800970130    2C                   ENDIF
102900970206     C     KBLP6         READE     FNBLP06L                               31
103000970130    1C                   ENDDO
103100970129     C**
103200970129     C** III° CICLO --> DUC > DATA ELABORAZIONE; DBR <= DATA ELABORAZ.
103300970129     C**
103400970129     C                   Z-ADD     DS1ADA        BLPDUC
103500970129     C     KBLP6B        SETGT     FNBLP06L
103600970129     C     COMFIL        READE     FNBLP06L                               31
103700970130    1C     *IN31         DOWEQ     *OFF
103800970204     C*
103900970130    2C     BLPDBR        IFGT      DS1ADA
104000970129     C     KBLP6B        SETGT     FNBLP06L
104100970129     C                   GOTO      LEGBLP
104200970204     C                   ELSE
104300970204     C* ESCLUDO SE DBR = 0
104400970204     C     BLPDBR        IFEQ      *ZEROS
104500970204     C     KBLP6C        SETGT     FNBLP06L
104600970204     C                   GOTO      LEGBLP
104700970204     C                   END
104800970130    2C                   END
104900970129      * TEST SE E' RECUPERO
105000970129     C                   Z-ADD     1             WA
105100970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
105200970129     C   32              MOVEL     D3A(WA)       DS3A
105300970129     C  N32              CLEAR                   DS3A
105400970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
105500970130    2C     §3ARBL        IFNE      'R'
105600070525     C     BLPNFV        ANDNE     99999
105700070525     C     BLPNFV        ANDNE     99998
105800070525     C     BLPCBO        ANDNE     '£ '
105900070525     c     blppdr        andgt     0
106000970129     C                   EXSR      CSPE
106100970130    2C                   ENDIF
106200970129     C     LEGBLP        TAG
106300970203     C     COMFIL        READE     FNBLP06L                               31
106400970130    1C                   ENDDO
106500070523     c
106600070523     c                   else
106700070523     c                   exsr      CALSPEDNEW
106800070523     c                   endif
106900930924     C*
107000930924     C                   ENDSR
107100070523     C*****************************************************************
107200070523     C* CALCOLO LE SPEDIZIONI A MODO NUOVO CONSIDERANDO LA DATA
107300070523     C*   RITIRO MERCE
107400070523     C*****************************************************************
107500070523     C     CALSPEDNEW    BEGSR
107600070523     C***
107700070523     C*ELABORO SPEDIZIONI
107800070523     C**
107900070523     C** I° CICLO --> DUC = 0 Drt <= DATA ELABORAZIONE
108000070523     C**
108100070523     C                   MOVE      *ZEROS        KDUC
108200070523     C                   MOVE      *ZEROS        KDrt
108300070523     C     KBLP5A        SETGT     FNBLP05L
108400070523     C     KBLP5         READE     FNBLP05L                               31
108500070523    1C     *IN31         DOWEQ     *OFF
108600070523     C     BLPDrt        ANDLE     DS1ADA
108700070523      * TEST SE E' RECUPERO
108800070523     C                   Z-ADD     1             WA
108900070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
109000070523     C   32              MOVEL     D3A(WA)       DS3A
109100070523     C  N32              CLEAR                   DS3A
109200070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
109300070523    2C     §3ARBL        IFNE      'R'
109400070523     C     BLPNFV        ANDNE     99999
109500070523     C     BLPNFV        ANDNE     99998
109600070523     C     BLPCBO        ANDNE     '£ '
109700070525     C     BLPpdr        ANDgt     0
109800070523     C                   EXSR      CSPE
109900070523    2C                   ENDIF
110000070523     C     KBLP5         READE     FNBLP05L                               31
110100070523    1C                   ENDDO
110200070523     C**
110300070523     C** II° CICLO --> DUC = DATA ELABORAZIONE
110400070523     C**
110500070523     C                   Z-ADD     DS1ADA        KDUC
110600070523     C     KBLP5         SETLL     FNBLP05L
110700070523     C     KBLP5         READE     FNBLP05L                               31
110800070523    1C     *IN31         DOWEQ     *OFF
110900070523      * TEST SE E' RECUPERO
111000070523     C                   Z-ADD     1             WA
111100070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
111200070523     C   32              MOVEL     D3A(WA)       DS3A
111300070523     C  N32              CLEAR                   DS3A
111400070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
111500070523    2C     §3ARBL        IFNE      'R'
111600070525     C     BLPNFV        ANDNE     99999
111700070525     C     BLPNFV        ANDNE     99998
111800070525     C     BLPCBO        ANDNE     '£ '
111900070523     C                   EXSR      CSPE
112000070523    2C                   ENDIF
112100070523     C     KBLP5         READE     FNBLP05L                               31
112200070523    1C                   ENDDO
112300070523     C**
112400070523     C** III° CICLO --> DUC > DATA ELABORAZIONE; Drt <= DATA ELABORAZ.
112500070523     C**
112600070523     C                   Z-ADD     DS1ADA        BLPDUC
112700070523     C     KBLP5B        SETGT     FNBLP05L
112800070523     C     COMFIL        READE     FNBLP05L                               31
112900070523    1C     *IN31         DOWEQ     *OFF
113000070523     C*
113100070523    2C     BLPDrt        IFGT      DS1ADA
113200070523     C     KBLP5B        SETGT     FNBLP05L
113300070523     C                   GOTO      LEGBLP5
113400070523   x2C                   ELSE
113500070523      * TEST SE E' RECUPERO
113600070523     C                   Z-ADD     1             WA
113700070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
113800070523     C   32              MOVEL     D3A(WA)       DS3A
113900070523     C  N32              CLEAR                   DS3A
114000070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
114100070523    3C     §3ARBL        IFNE      'R'
114200070525     C     BLPNFV        ANDNE     99999
114300070525     C     BLPNFV        ANDNE     99998
114400070525     C     BLPCBO        ANDNE     '£ '
114500070525     C     BLPpdr        ANDgt     0
114600070523     C                   EXSR      CSPE
114700070523    3C                   ENDIF
114800070523    2C                   ENDIF
114900070523     C     LEGBLP5       TAG
115000070523     C     COMFIL        READE     FNBLP05L                               31
115100070523    1C                   ENDDO
115200070523     C                   ENDSR
115300000831     C*****************************************************************
115400000831     C* ELABORO LE ANOMALIE 105 PER LA LINEA INTERESSATA
115500000831     C*****************************************************************
115600000831     C     ELA105        BEGSR
115700070523     C                   Z-ADD     105           WCAA
115800000831     C     KANM          SETLL     FNANM000
115900000831     C     KANM          READE     FNANM000                               32
116000000831     C*
116100000831     C     *IN32         DOWEQ     *OFF
116200000831     C* SE ANOMAIA CHIUSA VUOL DIRE CHE ADESSOHO LA BOLLA
116300000831     C     ANMDCH        IFEQ      0
116400040601     c     anmfle        andeq     simfel
116500000831     C* SE NON E' UGUALE ALLA LINEA DI PARTENZA, VEDO SE CONFERMABILE
116600000831     C*  DA ME
116700000831     C     ANMFLS        IFNE      COMFIL
116800000831     C* SE E' IN T06 NON MEMORIZZO, ALTRIMENTI MEMORIZZO PER SIMFEL
116900000831     C     ANMFLS        LOOKUP    T06                                    32
117000000831     C     *IN32         IFEQ      *OFF
117100000831     C     COMFIL        ANDEQ     SIMFEL
117200000831     C* NON SAPENDO IL TIPO SERVIZIO, SOMMO NELLA DISTRIBUZIONE
117300000831     C                   ADD       1             SKA(6)
117400000831     C                   ENDIF
117500000831     C*
117600000831     C                   ELSE
117700000831     C                   ADD       1             SKA(6)
117800000831     C                   ENDIF
117900000831     C                   ENDIF
118000000831     C*
118100000831     C     KANM          READE     FNANM000                               32
118200000831     C                   ENDDO
118300000831     C*
118400000831     C                   ENDSR
118500930924     C*****************************************************************
118600931007     C* SUBROUTINE DTBOL: DIFFERENZIA TRA FILIALE E C/SERVIZI
118700931013     C* ESEGUITA SOLO PER BOLLE NORMALI.
118800930927     C*****************************************************************
118900931007     C     DTBOL         BEGSR
119000950111     C*
119100000831     C* VERIFICO SE LNP POSTE
119200000831     C     BLPLNP        IFNE      SAVLNP
119300000831     C     BLPLNP        CHAIN     AZORG01L                           32
119400000831     C  N32              MOVEL     ORGDE3        OG143
119500000831     C   32              CLEAR                   OG143
119600020813     ***C                   MOVEL     §OGPT         LNPPT             1
119700020813     c                   if        §OGntw = 'PPT'
119800020813     c                   movel     'S'           LNPPT             1
119900020813     c                   else
120000020813     c                   clear                   LNPPT
120100020813     c                   endif
120200000831     C                   MOVEL     BLPLNP        SAVLNP
120300000831     C                   ENDIF
120400000831     C*
120500970129     C* MEMORIZZO SE E' DISK B O ALTRI
120600970129     C     BLPSOP        IFEQ      '§'
120700970129     C     BLPNRS        ANDGT     *ZEROS
120800970129     C     BLPSOP        OREQ      '%'
120900970129     C     BLPNRS        ANDGT     *ZEROS
121000970129     C                   SETON                                        05
121100970129     C                   ELSE
121200970129     C                   SETOFF                                       05
121300970129     C                   END
121400970129     C*
121500930927     C*TEST SE E' C/SERVIZI O FILIALE
121600931007     C     §3ARBL        IFEQ      'C'
121700930927     C                   SETON                                        04
121800930927     C                   ELSE
121900930927     C                   SETOFF                                       04
122000930927     C                   ENDIF
122100930927     C*
122200930927     C* SE TIPO SPEDIZIONE E' ESPRESSO
122300930927     C     BLPTSP        IFEQ      'E'
122400930927     C* SE (FILIALE) C/SERVIZI
122500930927     C   04              Z-ADD     2             X
122600930927     C* SE FILIALE
122700930927     C  N04              Z-ADD     5             X
122800950111     C                   ELSE
122900000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
123000950111     C     BLPTSP        IFEQ      'D'
123100000619     C     BLPTSP        OREQ      'P'
123200930927     C* SE (FILIALE) C/SERVIZI
123300930927     C   04              Z-ADD     3             X
123400930927     C* SE FILIALE
123500930927     C  N04              Z-ADD     6             X
123600950111     C                   ELSE
123700950111     C*
123800950111     C* SE (FILIALE) C/SERVIZI
123900950111     C   04              Z-ADD     1             X                 1 0
124000950111     C* SE FILIALE
124100950111     C  N04              Z-ADD     4             X
124200950111     C*
124300930927     C                   ENDIF
124400950111     C                   ENDIF
124500931007     C*
124600940411      * Preparo l'indice per c/servizio di filiale (dal 7 al 9), per le schiere
124700940411     C   04X             ADD       6             Y                 1 0
124800931007     C*
124900931007     C                   ENDSR
125000931007     C************************************************************
125100931007     C* SUBROUTINE DATBOL: REPERIMENTO DATI BOLLA PER CALCOLI.
125200931007     C*****************************************************************
125300931007     C     DATBOL        BEGSR
125400021220      *
125500021220      * Verifico i dati misurati con la macchina smistacolli
125600021220      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
125700021220      * e calcolo il VOLUME unitario medio per collo da usare se non
125800021220      * tutti i colli sono stati volumati dalla smistacolli
125900021220      *
126000030213      **** Verifico se sono stati volumati tutti i colli
126100021220     c                   clear                   Wvolu
126200050111     c                   if        BLPncr = BLPncl
126300021220     c                   z-add     BLPvlc        Wvolu
126400021220   x1c                   else
126500021220if  2c                   if        BLPvlc >= BLPvlf
126600021220     c                   z-add     BLPvlc        Wvolu
126700021220   x2c                   else
126800021220     c                   z-add     BLPvlf        Wvolu
126900021220e   2c                   endif
127000021220e   1c                   endif
127100021220      *
127200021220      * Calcolo il volume medio
127300021220     c     Wvolu         div(H)    BLPncl        wVOLUM
127400021220      *
127500021220      * Verifico i dati misurati con la macchina smistacolli
127600021220      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
127700021220      * e calcolo il PESO   unitario medio per collo da usare se non
127800021220      * tutti i colli sono stati pesati   dalla smistacolli
127900021220      *
128000030213      **** Verifico se sono stati pesati   tutti i colli
128100021220     c                   clear                   Wpeso
128200050111     c                   if        BLPncp = BLPncl
128300021220     c                   z-add     BLPpkc        Wpeso
128400021220   c1c                   else
128500021220if  2c                   if        BLPpkc >= BLPpkf
128600021220     c                   z-add     BLPpkc        Wpeso
128700021220   x2c                   else
128800021220     c                   z-add     BLPpkf        Wpeso
128900021220e   2c                   endif
129000021220e   1c                   endif
129100021220      *
129200021220      * Calcolo il peso medio
129300021220     c     Wpeso         div(H)    BLPncl        wPESCOL
129400940328     C*
129500930927     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
129600021213     C     Wpeso         IFLE      3500
129700930927     C                   SETON                                        06
129800930927     C                   ELSE
129900930927     C                   SETOFF                                       06
130000930927     C                   ENDIF
130100930927     C*
130200931006     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
130300021213     C                   z-add     Wpeso         PESTAX
130400930927     C*
130500930927     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
130600021213     C     Wvolu         MULT      §QTKPM        COMPKT
130700930927     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
130800930927     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
130900930927     C     COMPKT        IFGT      PESTAX
131000930927     C                   Z-ADD     COMPKT        PESTAX
131100930927     C                   END
131200930930     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
131300930927     C                   Z-ADD     0             COMPKT            8 0
131400930927     C     PESTAX        DIV       §3IARR        COMPKT
131500930927     C                   MVR                     RESTO             3 0
131600930927     C     RESTO         IFGT      0
131700930927     C     COMPKT        MULT      §3IARR        PESTAX
131800930927     C                   ADD       §3IARR        PESTAX
131900930930     C                   ENDIF
132000021213     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
132100021213     C     PESTAX        DIV(H)    BLPNCL        PESTAS
132200930927     C*
132300930927     C                   ENDSR
132400930927     C****************************************************************
132500930929     C* SUBROUTINE CALC:
132600930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
132700930927     C*****************************************************************
132800930927     C     CALC          BEGSR
132900930927     C*
133000930927     C******COLLI
133100930927     C                   ADD       1             SKA(X)
133200941012     C   04              ADD       1             SKA(Y)
133300930927     C*
133400930927     C******COLLI NON SPUNTATI IN PARTENZA: 88.
133500090304     C     ARTNPP        IFEQ      88
133600090304     C     ARTNPP        OREQ      89
133700090304     C     ARTNPP        OREQ      98
133800090304     C     ARTNPP        OREQ      96
133900970129     C* DISK B
134000970129     C     *IN05         IFEQ      *ON
134100970129     C*SE (FILIALE) C/SERVIZI
134200970129     C   04              ADD       1             PB88CS            7 0
134300970129     C   04              ADD       1             PB88              7 0
134400970129     C*SE FILIALE
134500970129     C  N04              ADD       1             PB88F             7 0
134600970129     C                   ELSE
134700970129     C* NO DISK B
134800970129     C*
134900930927     C*SE (FILIALE) C/SERVIZI
135000930927     C   04              ADD       1             PI88CS            7 0
135100941012     C   04              ADD       1             PS88              7 0
135200930927     C*SE FILIALE
135300930927     C  N04              ADD       1             PI88              7 0
135400970129     C                   END
135500930927     C                   ENDIF
135600950622     C******COLLI PISTOLA 87 (EX 88)
135700090304     C     ARTNPP        IFEQ      87
135800970129     C* DISK B
135900970129     C     *IN05         IFEQ      *ON
136000970129     C*SE (FILIALE) C/SERVIZI
136100970129     C   04              ADD       1             PB87CS            7 0
136200970129     C   04              ADD       1             PB87              7 0
136300970129     C*SE FILIALE
136400970129     C  N04              ADD       1             PB87F             7 0
136500970129     C                   ELSE
136600970129     C* NO DISK B
136700970129     C*
136800950622     C*SE (FILIALE) C/SERVIZI
136900950622     C   04              ADD       1             PI87CS            7 0
137000950622     C   04              ADD       1             PS87              7 0
137100950622     C*SE FILIALE
137200950622     C  N04              ADD       1             PI87              7 0
137300970129     C                   END
137400950622     C                   ENDIF
137500930927     C*
137600930927     C******PESO
137700930927     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
137800030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
137900030213      * Peso medio
138000030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
138100021220     c                   z-add     Wpescol       PESCOL
138200930927     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
138300930927     C   06              ADD       PESCOL        SKD(X)
138400941012     C   06
138500941012     CAN 04              ADD       PESCOL        SKD(Y)
138600970129     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
138700970129     C  N06              ADD       PESCOL        SKE(X)
138800970129     C  N06
138900970129     CAN 04              ADD       PESCOL        SKE(Y)
139000930927     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
139100930927     C                   ADD       PESCOL        SKF(X)
139200941012     C   04              ADD       PESCOL        SKF(Y)
139300930927     C*
139400930927     C******PESO TASSABILE
139500931007     C  N04
139600931007     CAN 06              ADD       PESTAS        PETS              9 1
139700931130      *
139800931130      ***** VOLUME
139900021220     *** * Se nel collo c'e' il volume utilizzo quello, altrimenti quello
140000021220     *** *  generico
140100021220     *** *
140200021220      * VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
140300030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
140400030213      * Volume medio
140500030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
140600021220     c                   z-add     Wvolum        Volum
140700021220     c                   add       VOLUM         SKI(X)
140800021220     c   04              add       VOLUM         SKI(Y)
140900931130      *
141000930927     C                   ENDSR
141100930927     C****************************************************************
141200931007     C* SUBROUTINE CSPE:  CALCOLO SPEDIZIONI
141300930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
141400930927     C*****************************************************************
141500931007     C     CSPE          BEGSR
141600930927     C*
141700931006     C*TEST SE E' C/SERVIZI O FILIALE
141800970228    1C     §3ARBL        IFEQ      'C'
141900931006     C*SE E' C/SERVIZI
142000931006     C                   SETON                                        04
142100970228   x1C                   ELSE
142200931006     C*SE E' FILIALE
142300931006     C                   SETOFF                                       04
142400970228    1C                   ENDIF
142500931006     C*
142600931006     C*FISSO GLI INDICI DI SCHIERA....
142700931006     C*
142800931006     C* SE TIPO SPEDIZIONE E' ESPRESSO
142900970228    1C     BLPTSP        IFEQ      'E'
143000931006     C* SE (FILIALE) C/SERVIZI
143100931006     C   04              Z-ADD     2             X
143200931006     C* SE FILIALE
143300931006     C  N04              Z-ADD     5             X
143400970228   X1C                   ELSE
143500000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
143600970228    2C     BLPTSP        IFEQ      'D'
143700000619    2C     BLPTSP        OREQ      'P'
143800931006     C* SE (FILIALE) C/SERVIZI
143900931006     C   04              Z-ADD     3             X
144000931006     C* SE FILIALE
144100931006     C  N04              Z-ADD     6             X
144200970228   x2C                   ELSE
144300950111     C*
144400950111     C* SE (FILIALE) C/SERVIZI
144500950111     C   04              Z-ADD     1             X                 1 0
144600950111     C* SE FILIALE
144700950111     C  N04              Z-ADD     4             X
144800950111     C*
144900970228    2C                   ENDIF
145000970228    1C                   ENDIF
145100940411      *
145200941012      * Se e' c/servizio aggiungo anche nel totale filiale c/serv.
145300941012     C   04X             ADD       6             Y
145400970228     C*
145500970206     C* Richiamo pgm per calcolo affidabilità
145600970303     C     BLPDCE        IFEQ      *ZEROS
145700970303     C     BLPDUC        ORNE      DS1ADA
145800970206     C                   MOVEL     ' '           D98TLA
145900970206     C                   MOVEL     'P'           D98TBO
146000970206     C                   MOVEL     BLPAAS        D98AAS
146100970206     C                   MOVEL     BLPLNP        D98LNP
146200970206     C                   MOVEL     BLPNRS        D98NRS
146300970206     C                   MOVEL     BLPNSP        D98NSP
146400970310     C                   Z-ADD     DS1ADA        D98DRF
146500970310     C                   Z-ADD     2359          D98ORF
146600970925     C                   CALL      'TNSD99R'
146700970925     C                   PARM                    DS99
146800970303     C                   ELSE
146900970303     C                   Z-ADD     BLPNPI        D98NPI
147000970303     C                   END
147100970206     C*
147200930927     C******SPEDIZIONI
147300970206    1C     BLPDUC        IFEQ      DS1ADA
147400970206     C*  PARTITO
147500930927     C                   ADD       1             SKL(X)
147600941012     C   04              ADD       1             SKL(Y)
147700970206     C*  PARTITO FUORI AFFIDABILITA'
147800970228     C* (SKO LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
147900970206    2C     D98NPI        IFGT      0
148000970206     C                   ADD       1             SKO(X)
148100970206     C   04              ADD       1             SKO(Y)
148200970206     C                   ADD       1             SKN(X)
148300970206     C   04              ADD       1             SKN(Y)
148400970206    2C                   END
148500970206   X1C                   ELSE
148600970206     C* A TERRA FUORI AFFIDABILITA'
148700970228     C* (SKP LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
148800970206    2C     D98NPI        IFGE      0
148900970206     C                   ADD       1             SKP(X)
149000970206     C   04              ADD       1             SKP(Y)
149100970206     C                   ADD       1             SKN(X)
149200970206     C   04              ADD       1             SKN(Y)
149300970206    2C                   END
149400970206    1C                   END
149500970129     C*  DA PARTIRE
149600970206     C                   MOVE      *BLANKS       COMDAP            1
149700970228     C* La distribuzione a terra del giorno non spareggiata non viene
149800970228     C* conteggiata nel "Da partire"
149900970303     C     BLPDUC        IFEQ      *ZEROS
150000970228     C     BLPDPC        ANDEQ     0
150100970206     C     D98NPI        ANDLT     0
150200970228     C*
150300970303     C     BLPDUC        OREQ      *ZEROS
150400970228     C     BLPDPC        ANDGT     DS1ADA
150500970228     C     D98NPI        ANDLT     0
150600970228     C*
150700970303     C     BLPDUC        ORGT      DS1ADA
150800970228     C     BLPDPC        ANDGT     DS1ADA
150900970228     C     D98NPI        ANDLT     0
151000970228     C*
151100970206     C                   MOVE      'N'           COMDAP
151200970206     C                   END
151300970206     C*
151400970206     C     COMDAP        IFEQ      *BLANKS
151500970129     C                   ADD       1             SKM(X)
151600970129     C   04              ADD       1             SKM(Y)
151700970206     C                   END
151800970228     C*
151900930927     C*
152000930927     C                   ENDSR
152100930927     C****************************************************************
152200930927     C* SUBROUTINE TOTLNP:
152300930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
152400930927     C*****************************************************************
152500930927     C     TOTLNP        BEGSR
152600950111     C*
152700930927     C***CALCOLO TOTALI COLLI
152800930927     C*TOTALE COLLI
152900930927     C     SKA(4)        ADD       SKA(5)        SKA456            7 0
153000930927     C                   ADD       SKA(6)        SKA456
153100930927     C**** **** **** **** **** **** **** **** ****
153200930927     C*
153300970129     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
153400930930     C     SKA456        IFNE      0
153500970129     C* DISK B
153600970203     C     PB88F         DIV(H)    SKA456        $XX               7 4
153700970129     C     $XX           MULT      100           PC88B             5 2
153800970129     C* ALTRI
153900970129     C     PI88          DIV(H)    SKA456        $XX
154000970129     C     $XX           MULT      100           PC88              5 2
154100970129     C* TOTALE
154200970129     C     PB88F         ADD       PI88          PI88T             7 0
154300970129     C     PI88T         DIV(H)    SKA456        $XX
154400970129     C     $XX           MULT      100           PC88T             5 2
154500930930     C                   ENDIF
154600931005     C*
154700970129     C                   CLEAR                   PER3T
154800970129     C     PB88F         IFNE      0
154900970129     C                   MOVE      '%'           PER3B             1
155000970129     C                   MOVE      '%'           PER3T             1
155100970129     C                   ELSE
155200970129     C                   MOVE      *BLANKS       PER3B
155300970129     C                   ENDIF
155400970129     C*
155500931005     C     PI88          IFNE      0
155600931005     C                   MOVE      '%'           PER3              1
155700970129     C                   MOVE      '%'           PER3T             1
155800931005     C                   ELSE
155900931005     C                   MOVE      *BLANKS       PER3
156000931005     C                   ENDIF
156100950622     C*
156200950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
156300950622     C     SKA456        IFNE      0
156400970129     C* DISK B
156500970129     C     PB87F         DIV(H)    SKA456        $XX
156600970129     C     $XX           MULT      100           PC87B             5 2
156700970129     C* ALTRI
156800950622     C     PI87          DIV(H)    SKA456        $XX
156900950622     C     $XX           MULT      100           PC87              5 2
157000970129     C* TOTALI
157100970129     C     PB87F         ADD       PI87          PI87T             7 0
157200970129     C     PI87T         DIV(H)    SKA456        $XX
157300970129     C     $XX           MULT      100           PC87T             5 2
157400950622     C                   ENDIF
157500950622     C*
157600970129     C                   CLEAR                   PER34T
157700950622     C     PI87          IFNE      0
157800950622     C                   MOVE      '%'           PER34             1
157900970129     C                   MOVE      '%'           PER34T            1
158000950622     C                   ELSE
158100950622     C                   MOVE      *BLANKS       PER34
158200950622     C                   ENDIF
158300970129     C     PB87F         IFNE      0
158400970129     C                   MOVE      '%'           PER34B            1
158500970129     C                   MOVE      '%'           PER34T            1
158600970129     C                   ELSE
158700970129     C                   MOVE      *BLANKS       PER34B
158800970129     C                   ENDIF
158900930927     C**** **** **** **** **** **** **** **** ****
159000930927     C***CALCOLO TOTALI PESO
159100930927     C*TOTALE PESO FINO A 35 QUINTALI
159200930927     C     SKD(4)        ADD       SKD(5)        SKD456            8 1
159300930927     C                   ADD       SKD(6)        SKD456
159400930927     C*TOTALE PESO OLTRE  35 QUINTALI
159500930927     C     SKE(4)        ADD       SKE(5)        SKE456            8 1
159600930927     C                   ADD       SKE(6)        SKE456
159700930927     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
159800930927     C     SKF(4)        ADD       SKF(5)        SKF456            8 1
159900930927     C                   ADD       SKF(6)        SKF456
160000930927     C**** **** **** **** **** **** **** **** ****
160100970129     C***CALCOLO TOTALE VOLUME
160200930927     C*TOTALE VOLUMI
160300941005     C     SKI(4)        ADD       SKI(5)        SKI456            9 4
160400930927     C                   ADD       SKI(6)        SKI456
160500930927     C**** **** **** **** **** **** **** **** ****
160600930927     C****CALCOLO % E TOTALI SPEDIZIONI
160700970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
160800970226     C* DA PARTIRE E FUORI AFFIDABILITA'
160900970226     C     WFES          IFEQ      'F'
161000970226     C     SKA(4)        ANDEQ     *ZEROS
161100970226     C     SKA(5)        ANDEQ     *ZEROS
161200970226     C     SKA(6)        ANDEQ     *ZEROS
161300970226     C                   CLEAR                   SKM(4)
161400970226     C                   CLEAR                   SKM(5)
161500970226     C                   CLEAR                   SKM(6)
161600970226     C                   CLEAR                   SKN(4)
161700970226     C                   CLEAR                   SKN(5)
161800970226     C                   CLEAR                   SKN(6)
161900970226     C                   CLEAR                   SKP(4)
162000970226     C                   CLEAR                   SKP(5)
162100970226     C                   CLEAR                   SKP(6)
162200970226     C                   END
162300930927     C***CALCOLO TOTALI SPEDIZIONI
162400970129     C*TOTALE SPEDIZIONI PARTITE
162500930927     C     SKL(4)        ADD       SKL(5)        SKL456            7 0
162600930927     C                   ADD       SKL(6)        SKL456
162700970305     C*TOTALE SPEDIZIONI DA PARTIRE
162800970129     C     SKM(4)        ADD       SKM(5)        SKM456            7 0
162900970129     C                   ADD       SKM(6)        SKM456
163000930927     C**** **** **** **** **** **** **** **** ****
163100930928     C****STAMPA SINGOLA FILIALE :DETTAGLIO E TOTALE.
163200970226     C*
163300931011     C     *INOF         IFEQ      *ON
163400941012     C  N36              EXCEPT    TESTA
163500020813     C                   Z-ADD     1             XD
163600931011     C                   ENDIF
163700931011     C*
163800941012     C  N36              EXCEPT    DETFIL
163900950210     C     DS1RIS        IFNE      'R'
164000941012     C                   EXSR      WRIFIL
164100941012     C                   ENDIF
164200930928     C**** **** **** **** **** **** **** **** ****
164300930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE, IN CAMPI AREA
164400000616     C** SOLO SE DEVO STAMPARE
164500000616     C     DS1RIS        IFNE      ' '
164600930929     C                   ADD       SKA456        AKA123
164700930929     C                   ADD       SKA(4)        AKA(1)
164800930929     C                   ADD       SKA(5)        AKA(2)
164900930929     C                   ADD       SKA(6)        AKA(3)
165000930929     C*
165100930929     C                   ADD       PI88          ARPI88            7 0
165200970129     C                   ADD       PB88F         ARPB88            7 0
165300970130     C                   ADD       PI88T         ARP88T            7 0
165400950622     C                   ADD       PI87          ARPI87            7 0
165500970130     C                   ADD       PB87F         ARPB87            7 0
165600970130     C                   ADD       PI87T         ARP87T            7 0
165700930929     C*
165800930929     C                   ADD       SKD456        AKD123
165900930929     C                   ADD       SKD(4)        AKD(1)
166000930929     C                   ADD       SKD(5)        AKD(2)
166100930929     C                   ADD       SKD(6)        AKD(3)
166200930929     C*
166300930929     C                   ADD       SKE456        AKE123
166400930929     C                   ADD       SKE(4)        AKE(1)
166500930929     C                   ADD       SKE(5)        AKE(2)
166600930929     C                   ADD       SKE(6)        AKE(3)
166700930929     C*
166800930929     C                   ADD       SKF456        AKF123
166900930929     C                   ADD       SKF(4)        AKF(1)
167000930929     C                   ADD       SKF(5)        AKF(2)
167100930929     C                   ADD       SKF(6)        AKF(3)
167200930929     C*
167300931007     C                   ADD       PETS          ARPETS            9 1
167400930929     C*
167500930929     C                   ADD       SKI456        AKI123
167600930929     C                   ADD       SKI(4)        AKI(1)
167700930929     C                   ADD       SKI(5)        AKI(2)
167800930929     C                   ADD       SKI(6)        AKI(3)
167900970129     C*
168000930929     C                   ADD       SKL456        AKL123
168100930929     C                   ADD       SKL(4)        AKL(1)
168200930929     C                   ADD       SKL(5)        AKL(2)
168300930929     C                   ADD       SKL(6)        AKL(3)
168400930929     C*
168500930929     C                   ADD       SKM456        AKM123
168600930929     C                   ADD       SKM(4)        AKM(1)
168700930929     C                   ADD       SKM(5)        AKM(2)
168800930929     C                   ADD       SKM(6)        AKM(3)
168900930929     C*
169000930929     C                   ADD       SKN(4)        AKN(1)
169100930929     C                   ADD       SKN(5)        AKN(2)
169200930929     C                   ADD       SKN(6)        AKN(3)
169300000616     C                   ENDIF
169400930929     C*
169500930929     C**** **** **** **** **** **** **** **** ****
169600930929     C****PULIZIA CAMPI FILIALE (DI TOTALE E DI DETTAGLIO)
169700930929     C*CAMPI DI TOTALE
169800930929     C                   Z-ADD     0             SKA456
169900930929     C                   Z-ADD     0             PI88
170000941012     C                   Z-ADD     0             PS88
170100930929     C                   Z-ADD     0             PC88
170200970129     C                   Z-ADD     0             PB88F
170300970129     C                   Z-ADD     0             PB88
170400970129     C                   Z-ADD     0             PC88B
170500970130     C                   Z-ADD     0             PI88T
170600970129     C                   Z-ADD     0             PC88T
170700950622     C                   Z-ADD     0             PI87
170800950622     C                   Z-ADD     0             PS87
170900950622     C                   Z-ADD     0             PC87
171000970129     C                   Z-ADD     0             PB87F
171100970129     C                   Z-ADD     0             PB87
171200970129     C                   Z-ADD     0             PC87B
171300970130     C                   Z-ADD     0             PI87T
171400970129     C                   Z-ADD     0             PC87T
171500930929     C                   Z-ADD     0             SKD456
171600930929     C                   Z-ADD     0             SKE456
171700930929     C                   Z-ADD     0             SKF456
171800930929     C                   Z-ADD     0             PETS
171900930929     C                   Z-ADD     0             SKI456
172000930929     C                   Z-ADD     0             SKL456
172100930929     C                   Z-ADD     0             SKM456
172200930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
172300930929     C*PER LA STAMPA).
172400931005     C                   MOVEA     *ZEROS        SKA(4)
172500930929     C*
172600931005     C                   MOVEA     *ZEROS        SKD(4)
172700930929     C*
172800931005     C                   MOVEA     *ZEROS        SKE(4)
172900930929     C*
173000931005     C                   MOVEA     *ZEROS        SKF(4)
173100930929     C*
173200931005     C                   MOVEA     *ZEROS        SKI(4)
173300930929     C*
173400931005     C                   MOVEA     *ZEROS        SKL(4)
173500930929     C*
173600931005     C                   MOVEA     *ZEROS        SKM(4)
173700930929     C*
173800931005     C                   MOVEA     *ZEROS        SKN(4)
173900970206     C                   MOVEA     *ZEROS        SKO(4)
174000970206     C                   MOVEA     *ZEROS        SKP(4)
174100930929     C*
174200930927     C                   ENDSR
174300930929     C****************************************************************
174400930929     C* SUBROUTINE TOTCS:
174500930929     C**CALCOLO %, CALCOLO TOTALI C/SERVIZI, STAMPA DETTAGLIO C/SERVIZI,
174600930929     C*****************************************************************
174700930929     C     TOTCS         BEGSR
174800950111     C*
174900930929     C***CALCOLO TOTALI COLLI
175000930929     C*TOTALE COLLI
175100930929     C     SKA(1)        ADD       SKA(2)        SKA123            7 0
175200930929     C                   ADD       SKA(3)        SKA123
175300930929     C**** **** **** **** **** **** **** **** ****
175400970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
175500930930     C     SKA123        IFNE      0
175600970130     C* DISK B
175700970130     C     PB88CS        DIV(H)    SKA123        $XX
175800970130     C     $XX           MULT      100           PC88CB            5 2
175900970130     C* ALTRI
176000931005     C     PI88CS        DIV(H)    SKA123        $XX
176100931005     C     $XX           MULT      100           PC88CS            5 2
176200970130     C* TOTALE
176300970130     C     PB88CS        ADD       PI88CS        P88CST            7 0
176400970130     C     P88CST        DIV(H)    SKA123        $XX
176500970130     C     $XX           MULT      100           PC88CT            5 2
176600930930     C                   ENDIF
176700931005     C*
176800970130     C                   CLEAR                   PER3T
176900970130     C     PB88CS        IFNE      0
177000970130     C                   MOVE      '%'           PER3B
177100970130     C                   MOVE      '%'           PER3T
177200931005     C                   ELSE
177300970130     C                   MOVE      *BLANKS       PER3B
177400931005     C                   ENDIF
177500970130     C     PI88CS        IFNE      0
177600970130     C                   MOVE      '%'           PER3
177700970130     C                   MOVE      '%'           PER3T
177800970130     C                   ELSE
177900970130     C                   MOVE      *BLANKS       PER3
178000970130     C                   ENDIF
178100950622     C****CALCOLO % COLLI NON SPUNTATI CON PISTOLA 87 (EX 88)
178200950622     C     SKA123        IFNE      0
178300970130     C* DISK B
178400970130     C     PB87CS        DIV(H)    SKA123        $XX
178500970130     C     $XX           MULT      100           PC87CB            5 2
178600970130     C* ALTRI
178700950622     C     PI87CS        DIV(H)    SKA123        $XX
178800950622     C     $XX           MULT      100           PC87CS            5 2
178900970130     C* TOTALI
179000970130     C     PB87CS        ADD       PI87CS        P87CST            7 0
179100970203     C     P87CST        DIV(H)    SKA123        $XX
179200970130     C     $XX           MULT      100           PC87CT            5 2
179300950622     C                   ENDIF
179400950622     C*
179500970130     C                   CLEAR                   PER34T
179600970130     C     PB87CS        IFNE      0
179700970130     C                   MOVE      '%'           PER34B
179800970130     C                   MOVE      '%'           PER34T
179900950622     C                   ELSE
180000970130     C                   MOVE      *BLANKS       PER34B
180100950622     C                   ENDIF
180200970130     C     PI87CS        IFNE      0
180300970130     C                   MOVE      '%'           PER34
180400970130     C                   MOVE      '%'           PER34T
180500970130     C                   ELSE
180600970130     C                   MOVE      *BLANKS       PER34
180700970130     C                   ENDIF
180800930929     C**** **** **** **** **** **** **** **** ****
180900930929     C***CALCOLO TOTALI PESO
181000930929     C*TOTALE PESO FINO A 35 QUINTALI
181100930929     C     SKD(1)        ADD       SKD(2)        SKD123            8 1
181200930929     C                   ADD       SKD(3)        SKD123
181300930929     C*TOTALE PESO OLTRE  35 QUINTALI
181400930929     C     SKE(1)        ADD       SKE(2)        SKE123            8 1
181500930929     C                   ADD       SKE(3)        SKE123
181600930929     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
181700930929     C     SKF(1)        ADD       SKF(2)        SKF123            8 1
181800930929     C                   ADD       SKF(3)        SKF123
181900930929     C**** **** **** **** **** **** **** **** ****
182000930929     C***CALCOLO TOTALI VOLUMI
182100930929     C*TOTALE VOLUMI
182200941005     C     SKI(1)        ADD       SKI(2)        SKI123            9 4
182300930929     C                   ADD       SKI(3)        SKI123
182400930929     C**** **** **** **** **** **** **** **** ****
182500930929     C****CALCOLO % E TOTALI SPEDIZIONI
182600970130     C***CALCOLO % SPEDIZIONI FUORI AFFIDABILITA'
182700970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
182800970226     C* DA PARTIRE E FUORI AFFIDABILITA'
182900970226     C     WFES          IFEQ      'F'
183000970226     C     SKA(1)        ANDEQ     *ZEROS
183100970226     C     SKA(2)        ANDEQ     *ZEROS
183200970226     C     SKA(3)        ANDEQ     *ZEROS
183300970226     C                   CLEAR                   SKM(1)
183400970226     C                   CLEAR                   SKM(2)
183500970226     C                   CLEAR                   SKM(3)
183600970226     C                   CLEAR                   SKN(1)
183700970226     C                   CLEAR                   SKN(2)
183800970226     C                   CLEAR                   SKN(3)
183900970226     C                   CLEAR                   SKP(1)
184000970226     C                   CLEAR                   SKP(2)
184100970226     C                   CLEAR                   SKP(3)
184200970226     C                   END
184300930929     C*
184400930929     C***CALCOLO TOTALI SPEDIZIONI
184500970130     C*TOTALE SPEDIZIONI PARTITE
184600930929     C     SKL(1)        ADD       SKL(2)        SKL123            7 0
184700930929     C                   ADD       SKL(3)        SKL123
184800970130     C*TOTALE SPEDIZIONI DA PARTIRE
184900930929     C     SKM(1)        ADD       SKM(2)        SKM123            7 0
185000930929     C                   ADD       SKM(3)        SKM123
185100930929     C**** **** **** **** **** **** **** **** ****
185200930929     C****STAMPA FILIALE C/SERVIZI: DETTAGLIO E TOTALE.
185300931011     C     *INOF         IFEQ      *ON
185400941012     C  N36              EXCEPT    TESTA
185500020813     C                   Z-ADD     0             XD
185600931011     C                   ENDIF
185700931011     C*
185800941012     C  N36              EXCEPT    DETCS
185900930929     C**** **** **** **** **** **** **** **** ****
186000930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE C/SERVIZI, IN CAMPI AREA
186100000616     C* SOLO SE STAMPO
186200000616     C     DS1RIS        IFNE      ' '
186300930930     C                   ADD       SKA123        AKA123            7 0
186400930929     C                   ADD       SKA(1)        AKA(1)
186500930929     C                   ADD       SKA(2)        AKA(2)
186600930929     C                   ADD       SKA(3)        AKA(3)
186700930929     C*
186800930929     C                   ADD       PI88CS        ARPI88            7 0
186900970130     C                   ADD       PB88CS        ARPB88            7 0
187000950622     C                   ADD       PI87CS        ARPI87            7 0
187100970130     C                   ADD       PB87CS        ARPB87            7 0
187200970130     C                   ADD       P88CST        ARP88T            7 0
187300970130     C                   ADD       P87CST        ARP87T            7 0
187400930929     C*
187500930930     C                   ADD       SKD123        AKD123            8 1
187600930929     C                   ADD       SKD(1)        AKD(1)
187700930929     C                   ADD       SKD(2)        AKD(2)
187800930929     C                   ADD       SKD(3)        AKD(3)
187900930929     C*
188000930930     C                   ADD       SKE123        AKE123            8 1
188100930929     C                   ADD       SKE(1)        AKE(1)
188200930929     C                   ADD       SKE(2)        AKE(2)
188300930929     C                   ADD       SKE(3)        AKE(3)
188400930929     C*
188500930930     C                   ADD       SKF123        AKF123            8 1
188600930929     C                   ADD       SKF(1)        AKF(1)
188700930929     C                   ADD       SKF(2)        AKF(2)
188800930929     C                   ADD       SKF(3)        AKF(3)
188900930929     C*
189000941005     C                   ADD       SKI123        AKI123            9 4
189100930929     C                   ADD       SKI(1)        AKI(1)
189200930929     C                   ADD       SKI(2)        AKI(2)
189300930929     C                   ADD       SKI(3)        AKI(3)
189400930929     C*
189500930930     C                   ADD       SKL123        AKL123            7 0
189600930930     C                   ADD       SKL(1)        AKL(1)
189700930929     C                   ADD       SKL(2)        AKL(2)
189800930929     C                   ADD       SKL(3)        AKL(3)
189900930929     C*
190000930930     C                   ADD       SKM123        AKM123            7 0
190100930929     C                   ADD       SKM(1)        AKM(1)
190200930929     C                   ADD       SKM(2)        AKM(2)
190300930929     C                   ADD       SKM(3)        AKM(3)
190400930929     C*
190500930929     C                   ADD       SKN(1)        AKN(1)
190600930929     C                   ADD       SKN(2)        AKN(2)
190700930929     C                   ADD       SKN(3)        AKN(3)
190800000616     C                   ENDIF
190900930929     C*
191000930929     C**** **** **** **** **** **** **** **** ****
191100930929     C****PULIZIA CAMPI FILIALE C/SERVIZI (DI TOTALE E DI DETTAGLIO)
191200930929     C*CAMPI DI TOTALE
191300930929     C                   Z-ADD     0             SKA123
191400930929     C                   Z-ADD     0             PI88CS
191500970130     C                   Z-ADD     0             PB88CS
191600930929     C                   Z-ADD     0             PC88CS
191700970130     C                   Z-ADD     0             PC88CB
191800970130     C                   Z-ADD     0             P88CST
191900970130     C                   Z-ADD     0             PC88CT
192000950622     C                   Z-ADD     0             PI87CS
192100970130     C                   Z-ADD     0             PB87CS
192200950622     C                   Z-ADD     0             PC87CS
192300970130     C                   Z-ADD     0             PC87CB
192400970130     C                   Z-ADD     0             P87CST
192500970130     C                   Z-ADD     0             PC87CT
192600930929     C                   Z-ADD     0             SKD123
192700930929     C                   Z-ADD     0             SKE123
192800930929     C                   Z-ADD     0             SKF123
192900930929     C                   Z-ADD     0             SKI123
193000930929     C                   Z-ADD     0             SKL123
193100930929     C                   Z-ADD     0             SKM123
193200930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
193300930929     C*PER LA STAMPA).
193400930929     C*
193500931005     C                   MOVEA     *ZEROS        SKA(1)
193600930929     C*
193700931005     C                   MOVEA     *ZEROS        SKD(1)
193800930929     C*
193900931005     C                   MOVEA     *ZEROS        SKE(1)
194000930929     C*
194100931005     C                   MOVEA     *ZEROS        SKF(1)
194200930929     C*
194300931005     C                   MOVEA     *ZEROS        SKI(1)
194400930929     C*
194500931005     C                   MOVEA     *ZEROS        SKL(1)
194600930929     C*
194700931005     C                   MOVEA     *ZEROS        SKM(1)
194800930929     C*
194900931005     C                   MOVEA     *ZEROS        SKN(1)
195000970206     C                   MOVEA     *ZEROS        SKO(1)
195100970206     C                   MOVEA     *ZEROS        SKP(1)
195200930929     C*
195300930929     C                   ENDSR
195400930929     C****************************************************************
195500930929     C* SUBROUTINE TOTARE:
195600930929     C**CALCOLO %, CALCOLO TOTALI AREA, STAMPA DETTAGLIO AREA,
195700930929     C*****************************************************************
195800930929     C     TOTARE        BEGSR
195900970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
196000930930     C     AKA123        IFNE      0
196100970130     C* DISK B
196200970130     C     ARPB88        DIV(H)    AKA123        $XX
196300970130     C     $XX           MULT      100           APC88B            5 2
196400970130     C* ALTRI
196500970130     C     ARPI88        DIV(H)    AKA123        $XX
196600970130     C     $XX           MULT      100           ARPC88            5 2
196700970130     C* TOTALE
196800970130     C     ARP88T        DIV(H)    AKA123        $XX
196900970130     C     $XX           MULT      100           APC88T            5 2
197000930930     C                   ENDIF
197100931005     C*
197200970130     C                   CLEAR                   PER3T
197300970130     C     ARPB88        IFNE      0
197400970130     C                   MOVE      '%'           PER3B
197500970130     C                   MOVE      '%'           PER3T
197600931005     C                   ELSE
197700970130     C                   MOVE      *BLANKS       PER3B
197800931005     C                   ENDIF
197900970130     C     ARPI88        IFNE      0
198000970130     C                   MOVE      '%'           PER3
198100970130     C                   MOVE      '%'           PER3T
198200970130     C                   ELSE
198300970130     C                   MOVE      *BLANKS       PER3
198400970130     C                   ENDIF
198500950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
198600950622     C     AKA123        IFNE      0
198700970130     C* DISK B
198800970130     C     ARPB87        DIV(H)    AKA123        $XX
198900970130     C     $XX           MULT      100           APC87B            5 2
199000970130     C* ALTRI
199100950622     C     ARPI87        DIV(H)    AKA123        $XX
199200950622     C     $XX           MULT      100           ARPC87            5 2
199300970130     C* TOTALE
199400970130     C     ARP87T        DIV(H)    AKA123        $XX
199500970130     C     $XX           MULT      100           APC87T            5 2
199600950622     C                   ENDIF
199700950622     C*
199800970130     C                   CLEAR                   PER34T
199900970130     C     ARPB87        IFNE      0
200000970130     C                   MOVE      '%'           PER34B
200100970130     C                   MOVE      '%'           PER34T
200200950622     C                   ELSE
200300970130     C                   MOVE      *BLANKS       PER34B
200400950622     C                   ENDIF
200500970130     C     ARPI87        IFNE      0
200600970130     C                   MOVE      '%'           PER34
200700970130     C                   MOVE      '%'           PER34T
200800970130     C                   ELSE
200900970130     C                   MOVE      *BLANKS       PER34
201000970130     C                   ENDIF
201100930929     C**** **** **** **** **** **** **** **** ****
201200930929     C****STAMPA AREA :DETTAGLIO E TOTALE.
201300020813     C     XD            IFLE      7
201400941012     C  N36              EXCEPT    DETARE
201500931011     C                   ELSE
201600941012     C  N36              EXCEPT    TESTA
201700941012     C  N36              EXCEPT    DETARE
201800931011     C                   ENDIF
201900930929     C**** **** **** **** **** **** **** **** ****
202000931007     C****SOMMA CAMPI TOTALI AREA, IN CAMPI TOT. GENERALE
202100000616     C* SOLO SE  STAMPO
202200000616     C     DS1RIS        IFNE      ' '
202300931006     C*
202400931008     C                   ADD       AKA123        TKA123            7 0
202500931006     C                   ADD       AKA(1)        TKA(1)
202600931006     C                   ADD       AKA(2)        TKA(2)
202700931006     C                   ADD       AKA(3)        TKA(3)
202800931006     C*
202900970130     C                   ADD       ARP88T        TGP88T            7 0
203000970130     C                   ADD       ARP87T        TGP87T            7 0
203100931006     C*
203200931008     C                   ADD       AKD123        TKD123            8 1
203300931006     C                   ADD       AKD(1)        TKD(1)
203400931006     C                   ADD       AKD(2)        TKD(2)
203500931006     C                   ADD       AKD(3)        TKD(3)
203600931006     C*
203700931008     C                   ADD       AKE123        TKE123            8 1
203800931006     C                   ADD       AKE(1)        TKE(1)
203900931006     C                   ADD       AKE(2)        TKE(2)
204000931006     C                   ADD       AKE(3)        TKE(3)
204100931006     C*
204200931008     C                   ADD       AKF123        TKF123            8 1
204300931006     C                   ADD       AKF(1)        TKF(1)
204400931006     C                   ADD       AKF(2)        TKF(2)
204500931006     C                   ADD       AKF(3)        TKF(3)
204600931006     C*
204700931007     C                   ADD       ARPETS        TGPETS            9 1
204800931006     C*
204900941005     C                   ADD       AKI123        TKI123            9 4
205000931006     C                   ADD       AKI(1)        TKI(1)
205100931006     C                   ADD       AKI(2)        TKI(2)
205200931006     C                   ADD       AKI(3)        TKI(3)
205300931006     C*
205400931008     C                   ADD       AKL123        TKL123            7 0
205500931006     C                   ADD       AKL(1)        TKL(1)
205600931006     C                   ADD       AKL(2)        TKL(2)
205700931006     C                   ADD       AKL(3)        TKL(3)
205800000616     C                   ENDIF
205900931006     C*
206000930930     C**** **** **** **** **** **** **** **** ****
206100930930     C****PULIZIA CAMPI AREA    (DI TOTALE E DI DETTAGLIO)
206200930929     C*CAMPI DI TOTALE
206300930930     C                   Z-ADD     0             AKA123
206400930930     C                   Z-ADD     0             ARPI88
206500970130     C                   Z-ADD     0             ARPB88
206600970130     C                   Z-ADD     0             APC88B
206700970130     C                   Z-ADD     0             ARPC88
206800970130     C                   Z-ADD     0             ARP88T
206900970130     C                   Z-ADD     0             APC88T
207000950622     C                   Z-ADD     0             ARPI87
207100970130     C                   Z-ADD     0             ARPB87
207200950622     C                   Z-ADD     0             ARPC87
207300970130     C                   Z-ADD     0             APC87B
207400970130     C                   Z-ADD     0             ARP87T
207500970130     C                   Z-ADD     0             APC87T
207600930930     C                   Z-ADD     0             AKD123
207700930930     C                   Z-ADD     0             AKE123
207800930930     C                   Z-ADD     0             AKF123
207900930930     C                   Z-ADD     0             ARPETS
208000930930     C                   Z-ADD     0             AKI123
208100930930     C                   Z-ADD     0             AKL123
208200930930     C                   Z-ADD     0             AKM123
208300930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
208400930929     C*PER LA STAMPA).
208500931005     C                   MOVEA     *ZEROS        AKA(1)
208600930929     C*
208700931005     C                   MOVEA     *ZEROS        AKD(1)
208800930929     C*
208900931005     C                   MOVEA     *ZEROS        AKE(1)
209000930929     C*
209100931005     C                   MOVEA     *ZEROS        AKF(1)
209200930929     C*
209300931005     C                   MOVEA     *ZEROS        AKI(1)
209400930929     C*
209500931005     C                   MOVEA     *ZEROS        AKL(1)
209600930929     C*
209700931005     C                   MOVEA     *ZEROS        AKM(1)
209800930929     C*
209900931005     C                   MOVEA     *ZEROS        AKN(1)
210000930929     C*
210100930929     C                   ENDSR
210200930929     C****************************************************************
210300931006     C* SUBROUTINE ELATRA: ELABORAZIONE TRANSITI
210400931006     C*****************************************************************
210500931006     C     ELATRA        BEGSR
210600950111     C*
210700931008     C                   Z-ADD     0             FLDTXT
210800950111     C*
210900950111     C* ?    C O L L I    T R A N S I T O    ?
211000950329     C*
211100931007     C*POSIZ.PER'DATA DA'(DATA USCITA DAL TRANSITO).
211200020823     C     KDTA          SETLL     FNBTT38L
211300020823     C     KDTA          READE     FNBTT38L                               31
211400931006     C* CICLO LETTURA COLLI
211500970130    1C     *IN31         DOWEQ     *OFF
211600931007     C*
211700931013     C* ENTRA SOLO AL CAMBIO BOLLA.
211800970130    2C     DSTXT1        IFNE      FLDTXT
211900950111     C                   Z-ADD     DSTXT1        FLDTXT
212000020823     C     KBTP11        CHAIN     FNBTP11L                           32
212100931130      *
212200931130      * SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
212300970130    3C     *IN32         IFEQ      *OFF
212400950111     C*
212500950111     C                   Z-ADD     1             WA
212600950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
212700950111     C   32              MOVEL     D3A(WA)       DS3A
212800950111     C  N32              CLEAR                   DS3A
212900931130      *
213000950111     C* REPERIM.DATI BOLLA, (CORR.,ESPR.,RAPID,NON DIFFERENZIANDO I C/SERV.)
213100931007     C* QUINDI SOLO PER BOLLE-TRANSITO.
213200931007     C                   EXSR      DTBOT
213300950111     C* REPERIMENTO DATI BOLLA, PER CALCOLO TRANSITI
213400950111     C                   EXSR      DATBOT
213500931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN RECUPERO PER FARLA SCARTARE
213600970130   X3C                   ELSE
213700931006     C                   MOVEL     'R'           §3ARBL
213800970130    3C                   ENDIF
213900970130    2C                   ENDIF
214000931006     C*
214100931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
214200970130    2C     §3ARBL        IFNE      'R'
214300931013     C* CALCOLI-TRANSITI
214400931007     C                   EXSR      CALCTR
214500970130    2C                   ENDIF
214600931006     C*
214700020823     C     KDTA          READE     FNBTT38L                               31
214800970130    1C                   ENDDO
214900970505     C* AGGIUNGO I COLLI IN DISGUIDO
215000970505     C                   ADD       WDIS88        TRPI88
215100000616     C                   CLEAR                   WDIS88
215200931006     C***
215300950111     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
215400950111     C* ?    S P E D I Z I O N I    T R A N S I T O    ?
215500931007     C*
215600950111     C* POSIZIONAMENTO PER DATA INIZIO PERIODO
215700020823     C     KDTA          SETLL     FNBTP15L
215800020823     C     KDTA          READE     FNBTP15L                               31
215900931013     C*
216000970130    1C     *IN31         DOWEQ     *OFF
216100931007     C*
216200931130      * TEST SE E' RECUPERO
216300950111     C                   Z-ADD     1             WA
216400950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
216500950111     C   32              MOVEL     D3A(WA)       DS3A
216600950111     C  N32              CLEAR                   DS3A
216700931006     C*
216800931006     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
216900970130    2C     §3ARBL        IFNE      'R'
217000931007     C                   EXSR      CSPETR
217100970130    2C                   ENDIF
217200931006     C*
217300950111     C*
217400931006     C*LETTURA DENTRO AL CICLO
217500020823     C     KDTA          READE     FNBTP15L                               31
217600970130    1C                   ENDDO
217700931006     C*
217800931006     C                   ENDSR
217900931007     C*****************************************************************
218000931007     C* SUBROUTINE DTBOT: NON DIFFERENZIA TRA FILIALE E C/SERVIZI
218100931007     C*****************************************************************
218200931007     C     DTBOT         BEGSR
218300950111     C*
218400931007     C*TEST SE E' C/SERVIZI O FILIALE
218500931007     C*MI SERVE SOLO PER SAPERE SE ADDIZIONARE O MENO IL PESO TASSABILE.
218600931007     C     §3ARBL        IFEQ      'C'
218700931007     C                   SETON                                        04
218800931007     C                   ELSE
218900931007     C                   SETOFF                                       04
219000931007     C                   ENDIF
219100931007     C*
219200931007     C*FISSO GLI INDICI DI SCHIERA....
219300931007     C*
219400931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
219500950111     C     BTPTSP        IFEQ      'E'
219600931007     C                   Z-ADD     2             X
219700950111     C                   ELSE
219800931007     C*
219900950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
220000950111     C     BTPTSP        IFEQ      'D'
220100931007     C                   Z-ADD     3             X
220200950111     C                   ELSE
220300950111     C                   Z-ADD     1             X
220400931007     C                   ENDIF
220500950111     C                   ENDIF
220600931007     C*
220700931007     C                   ENDSR
220800950111     C************************************************************
220900950111     C* SUBROUTINE DATBOT: REPERIMENTO DATI BOLLA PER CALCOLI (TRANSITI)
221000950111     C*****************************************************************
221100950111     C     DATBOT        BEGSR
221200021213     ***C*
221300021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
221400021213     ***C     BTPPKB        DIV(H)    BTPNCL        PESCOL            8 1
221500021213     *** *
221600021213     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
221700021213     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
221800021213     *** * BTPVLC=Volume rilevato)
221900050111     ***C     BTPNCR        IFEQ      BTPNCL
222000021213     ***C                   Z-ADD     BTPVLC        BTPVLB
222100021213     ***C                   ELSE
222200021213     ***C     BTPVLC        IFGE      BTPVLB
222300021213     ***C                   Z-ADD     BTPVLC        BTPVLB
222400021213     ***C                   ENDIF
222500021213     ***C                   ENDIF
222600021213     ***C*
222700021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
222800021213     ***C     BTPVLB        DIV(H)    BTPNCL        VOLUM            12 4
222900021220      *
223000021220      * Verifico i dati misurati con la macchina smistacolli
223100021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
223200021220      * e calcolo il VOLUME unitario medio per collo da usare se non
223300021220      * tutti i colli sono stati volumati dalla smistacolli
223400021220      *
223500030213      **** Verifico se sono stati volumati tutti i colli
223600030213     c***                clear                   Wsivol
223700021220     c                   clear                   Wvolu
223800050111     c                   IF        BTPncr = BTPncl
223900021220     c                   z-add     BTPvlc        Wvolu
224000030213     c***                eval      Wsivol =  'S'
224100021220   x1c                   else
224200021220if  2c                   if        BTPvlc >= BTPvlb
224300021220     c                   z-add     BTPvlc        Wvolu
224400021220   x2c                   else
224500021220     c                   z-add     BTPvlb        Wvolu
224600021220e   2c                   endif
224700021220e   1c                   endif
224800021220      *
224900021220      * Calcolo il volume medio
225000021220     c     Wvolu         div(H)    BTPncl        wVOLUM
225100021220      *
225200021220      * Verifico i dati misurati con la macchina smistacolli
225300021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
225400021220      * e calcolo il PESO   unitario medio per collo da usare se non
225500021220      * tutti i colli sono stati pesati   dalla smistacolli
225600021220      *
225700030213      **** Verifico se sono stati pesati   tutti i colli
225800030213     c***                clear                   Wsipes
225900021220     c                   clear                   Wpeso
226000050111     c                   If        BTPncp = BTPncl
226100021220     c                   z-add     BTPpkc        Wpeso
226200030213     c***                eval      Wsipes =  'S'
226300021220   x1c                   else
226400021220if  2c                   if        BTPpkc >= BTPpkb
226500021220     c                   z-add     BTPpkc        Wpeso
226600021220   x2c                   else
226700021220     c                   z-add     BTPpkb        Wpeso
226800021220e   2c                   endif
226900021220e   1c                   endif
227000021220      *
227100021220      * Calcolo il peso medio
227200021220     c     Wpeso         div(H)    BTPncl        wPESCOL
227300950111     C*
227400950111     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
227500021213     ***C     BTPPKB        IFLE      3500
227600021213     C     Wpeso         IFLE      3500
227700950111     C                   SETON                                        06
227800950111     C                   ELSE
227900950111     C                   SETOFF                                       06
228000950111     C                   ENDIF
228100950111     C*
228200950111     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
228300021213     ***C                   Z-ADD     BTPPKB        PESTAX            8 0
228400021213     C                   z-add     Wpeso         PESTAX
228500950111     C*
228600950111     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
228700021213     ***C     BTPVLB        MULT      §QTKPM        COMPKT
228800021213     C     Wvolu         MULT      §QTKPM        COMPKT
228900950111     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
229000950111     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
229100950111     C     COMPKT        IFGT      PESTAX
229200950111     C                   Z-ADD     COMPKT        PESTAX
229300950111     C                   END
229400950111     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
229500950111     C                   Z-ADD     0             COMPKT            8 0
229600950111     C     PESTAX        DIV       §3IARR        COMPKT
229700950111     C                   MVR                     RESTO             3 0
229800950111     C     RESTO         IFGT      0
229900950111     C     COMPKT        MULT      §3IARR        PESTAX
230000950111     C                   ADD       §3IARR        PESTAX
230100950111     C                   ENDIF
230200950111     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
230300021213     C     PESTAX        DIV(H)    BTPNCL        PESTAS
230400950111     C*
230500950111     C                   ENDSR
230600931007     C****************************************************************
230700931007     C* SUBROUTINE CALCTR:
230800931007     C* SOMMA VALORI COLLO, IN TRANSITI.
230900931007     C* COLLI;COLLI NON SPUNT;PESO;PESO TASSABILE;VOLUME.
231000931007     C*****************************************************************
231100931007     C     CALCTR        BEGSR
231200950111     C*
231300931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
231400931007     C******COLLI
231500931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
231600931007     C                   ADD       1             XKA(X)
231700931007     C*
231800931007     C******COLLI NON SPUNTATI IN PARTENZA: 88.
231900950111     C     BTTPUT        IFEQ      88
232000950331     C     BTTPUT        OREQ      89
232100970430     C     BTTPUT        OREQ      98
232200970430     C     BTTPUT        OREQ      96
232300941012     C  N04              ADD       1             TRPI88            7 0
232400931007     C                   ENDIF
232500950622     C******COLLI PISTOLA 87 (EX 88 E 89)
232600950622     C     BTTPUT        IFEQ      87
232700950622     C  N04              ADD       1             TRPI87            7 0
232800950622     C                   ENDIF
232900931007     C*
233000931007     C******PESO
233100931007     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
233200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
233300030213      * Peso medio
233400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
233500030213if  1c***                if        Wsipes = 'S'
233600030213     c***                z-add(H)  BTTpuc        PESCOL
233700030213   x1c***                else
233800021220     c                   z-add     Wpescol       PESCOL
233900030213e   1c***                endif
234000950403     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
234100931007     C   06              ADD       PESCOL        XKD(X)
234200931007     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
234300931007     C  N06              ADD       PESCOL        XKE(X)
234400931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
234500931007     C                   ADD       PESCOL        XKF(X)
234600931007     C*
234700931007     C*
234800931007     C******PESO TASSABILE
234900931007     C  N04
235000931007     CAN 06              ADD       PESTAS        TRPETS            9 1
235100931007     C*
235200931007     C*
235300931007     C******VOLUME
235400021213     ***C     BTTVUC        IFGT      0
235500021213     ***C                   Z-ADD(H)  BTTVUC        WVOLUM
235600021213     ***C                   ELSE
235700021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
235800021213     ***C                   ENDIF
235900021220     ***C*
236000021220     ***C                   ADD       WVOLUM        XKI(X)
236100030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
236200030213      * Volume medio
236300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
236400030213if  1c***                if        Wsivol = 'S'
236500030213     c***                z-add     BTTvuc        Volum
236600030213   x1c***                else
236700021220     c                   z-add     Wvolum        Volum
236800030213e   1c***                endif
236900021220     c                   add       VOLUM         XKI(X)
237000931007     C*
237100931007     C                   ENDSR
237200930929     C****************************************************************
237300931007     C* SUBROUTINE CSPETR:  CALCOLO SPEDIZIONI -TRANSITI
237400931007     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
237500931007     C*****************************************************************
237600931007     C     CSPETR        BEGSR
237700931007     C*
237800931007     C*FISSO GLI INDICI DI SCHIERA....
237900931007     C*
238000931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
238100950111     C     BTPTSP        IFEQ      'E'
238200931007     C                   Z-ADD     2             X
238300950111     C                   ELSE
238400931007     C*
238500950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
238600950111     C     BTPTSP        IFEQ      'D'
238700931007     C                   Z-ADD     3             X
238800950111     C                   ELSE
238900950111     C                   Z-ADD     1             X                 1 0
239000931007     C                   ENDIF
239100950111     C                   ENDIF
239200931007     C*
239300931007     C*
239400931007     C******SPEDIZIONI
239500931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
239600931007     C* SOMMO SOLO IN SCHIERA DEL TOTALE
239700931007     C                   ADD       1             XKL(X)
239800931007     C*
239900931007     C                   ENDSR
240000931007     C****************************************************************
240100931007     C* SUBROUTINE TOTTRA:
240200931007     C**CALCOLO TOTALI TRANSITI STAMPA DETTAGLIO TRANSITI
240300931007     C*****************************************************************
240400931007     C     TOTTRA        BEGSR
240500950111     C*
240600931007     C**** **** **** **** **** **** **** **** ****
240700970218     C* AGGIUNGO I COLLI IN DISGUIDO AL TOTALE COLLI CORRIERE
240800970218     C                   ADD       WDIS          XKA(1)
240900000616     C                   CLEAR                   WDIS
241000931007     C****CALCOLO TOTALE COLLI:
241100931007     C     XKA(1)        ADD       XKA(2)        XKA123            7 0
241200931008     C                   ADD       XKA(3)        XKA123
241300950331     C*
241400931007     C**** **** **** **** **** **** **** **** ****
241500931007     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
241600931007     C     XKA123        IFNE      0
241700931007     C     TRPI88        DIV(H)    XKA123        $XX
241800931007     C     $XX           MULT      100           TRPC88            5 2
241900931007     C                   ENDIF
242000931007     C*
242100931008     C     TRPI88        IFNE      0
242200931007     C                   MOVE      '%'           PER3              1
242300931007     C                   ELSE
242400931007     C                   MOVE      *BLANKS       PER3
242500931007     C                   ENDIF
242600950622     C****CALCOLO % COLLI PISTOLA 87 (EX 88 E 89)
242700950622     C     XKA123        IFNE      0
242800950622     C     TRPI87        DIV(H)    XKA123        $XX
242900950622     C     $XX           MULT      100           TRPC87            5 2
243000950622     C                   ENDIF
243100950622     C*
243200950622     C     TRPI87        IFNE      0
243300950622     C                   MOVE      '%'           PER34             1
243400950622     C                   ELSE
243500950622     C                   MOVE      *BLANKS       PER34
243600950622     C                   ENDIF
243700931007     C**** **** **** **** **** **** **** **** ****
243800931007     C****CALCOLO TOTALI PESO:
243900931007     C*
244000931007     C*TOTALE PESO FINO A 35 QUINTALI
244100931007     C     XKD(1)        ADD       XKD(2)        XKD123            8 1
244200931007     C                   ADD       XKD(3)        XKD123
244300931007     C*TOTALE PESO OLTRE  35 QUINTALI
244400931007     C     XKE(1)        ADD       XKE(2)        XKE123            8 1
244500931007     C                   ADD       XKE(3)        XKE123
244600931007     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
244700931007     C     XKF(1)        ADD       XKF(2)        XKF123            8 1
244800931007     C                   ADD       XKF(3)        XKF123
244900931007     C**** **** **** **** **** **** **** **** ****
245000931007     C****CALCOLO TOTALE VOLUME
245100941005     C     XKI(1)        ADD       XKI(2)        XKI123            9 4
245200931007     C                   ADD       XKI(3)        XKI123
245300931007     C**** **** **** **** **** **** **** **** ****
245400931007     C****CALCOLO TOTALE SPEDIZIONI
245500931007     C     XKL(1)        ADD       XKL(2)        XKL123            7 0
245600931007     C                   ADD       XKL(3)        XKL123
245700931007     C**** **** **** **** **** **** **** **** ****
245800931007     C****STAMPA TRANSITI :DETTAGLIO E TOTALE.
245900941012     C  N36              EXCEPT    DETTRA
246000950210     C     DS1RIS        IFNE      'R'
246100941012     C                   EXSR      WRITRA
246200941012     C                   ENDIF
246300931007     C**** **** **** **** **** **** **** **** ****
246400931007     C****SOMMA CAMPI TOTALI TRANSITI IN CAMPI TOTALI TOT.GEN.
246500000616     C**  SOLO SE NON STAMPO
246600000616     C     DS1RIS        IFNE      ' '
246700931007     C                   ADD       XKA123        TKA123
246800931007     C                   ADD       XKA(1)        TKA(1)
246900931007     C                   ADD       XKA(2)        TKA(2)
247000931007     C                   ADD       XKA(3)        TKA(3)
247100931007     C*
247200970130     C                   ADD       TRPI88        TGP88T            7 0
247300970130     C                   ADD       TRPI87        TGP87T            7 0
247400931007     C*
247500931007     C                   ADD       XKD123        TKD123
247600931007     C                   ADD       XKD(1)        TKD(1)
247700931007     C                   ADD       XKD(2)        TKD(2)
247800931007     C                   ADD       XKD(3)        TKD(3)
247900931007     C*
248000931007     C                   ADD       XKE123        TKE123
248100931007     C                   ADD       XKE(1)        TKE(1)
248200931007     C                   ADD       XKE(2)        TKE(2)
248300931007     C                   ADD       XKE(3)        TKE(3)
248400931007     C*
248500931007     C                   ADD       XKF123        TKF123
248600931007     C                   ADD       XKF(1)        TKF(1)
248700931007     C                   ADD       XKF(2)        TKF(2)
248800931007     C                   ADD       XKF(3)        TKF(3)
248900931007     C*
249000931007     C                   ADD       TRPETS        TGPETS            9 1
249100931007     C*
249200931007     C                   ADD       XKI123        TKI123
249300931007     C                   ADD       XKI(1)        TKI(1)
249400931007     C                   ADD       XKI(2)        TKI(2)
249500931007     C                   ADD       XKI(3)        TKI(3)
249600931007     C*
249700931007     C                   ADD       XKL123        TKL123
249800931007     C                   ADD       XKL(1)        TKL(1)
249900931007     C                   ADD       XKL(2)        TKL(2)
250000931007     C                   ADD       XKL(3)        TKL(3)
250100000616     C                   ENDIF
250200931007     C*
250300931007     C**** **** **** **** **** **** **** **** ****
250400931008     C****PULIZIA CAMPI TRANSITO (DI TOTALE E DI DETTAGLIO)
250500931007     C*CAMPI DI TOTALE
250600931007     C                   Z-ADD     0             XKA123
250700931007     C                   Z-ADD     0             TRPI88
250800931007     C                   Z-ADD     0             TRPC88
250900950622     C                   Z-ADD     0             TRPI87
251000950622     C                   Z-ADD     0             TRPC87
251100931007     C                   Z-ADD     0             XKD123
251200931007     C                   Z-ADD     0             XKE123
251300931007     C                   Z-ADD     0             XKF123
251400931007     C                   Z-ADD     0             TRPETS
251500931007     C                   Z-ADD     0             XKI123
251600931007     C                   Z-ADD     0             XKL123
251700931007     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
251800931007     C*PER LA STAMPA).
251900931007     C                   MOVEA     *ZEROS        XKA(1)
252000931007     C*
252100931007     C                   MOVEA     *ZEROS        XKD(1)
252200931007     C*
252300931007     C                   MOVEA     *ZEROS        XKE(1)
252400931007     C*
252500931007     C                   MOVEA     *ZEROS        XKF(1)
252600931007     C*
252700931007     C                   MOVEA     *ZEROS        XKI(1)
252800931007     C*
252900931007     C                   MOVEA     *ZEROS        XKL(1)
253000931007     C*
253100931007     C                   ENDSR
253200931007     C****************************************************************
253300931008     C* SUBROUTINE TOTGEN:
253400931008     C**CALCOLO TOTALI TOT.GEN. STAMPA DETTAGLIO TOT.GEN.
253500931008     C*****************************************************************
253600931008     C     TOTGEN        BEGSR
253700950111     C*
253800931008     C**** **** **** **** **** **** **** **** ****
253900931008     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
254000931008     C     TKA123        IFNE      0
254100970130     C* TOTALE
254200970131     C     TGP88T        DIV(H)    TKA123        $XX
254300970131     C     $XX           MULT      100           TGC88T            5 2
254400931008     C                   ENDIF
254500931008     C*
254600970131     C     TGP88T        IFNE      0
254700970131     C                   MOVE      '%'           PER3              1
254800970131     C                   ELSE
254900970131     C                   MOVE      *BLANKS       PER3
255000970131     C                   ENDIF
255100950622     C****CALCOLO % COLLI CON PISTOLA 87 (EX 88 E 89)
255200950622     C     TKA123        IFNE      0
255300970130     C* TOTALE
255400970131     C     TGP87T        DIV(H)    TKA123        $XX
255500970131     C     $XX           MULT      100           TGC87T            5 2
255600950622     C                   ENDIF
255700950622     C*
255800970131     C     TGP87T        IFNE      0
255900950622     C                   MOVE      '%'           PER34             1
256000950622     C                   ELSE
256100950622     C                   MOVE      *BLANKS       PER34
256200950622     C                   ENDIF
256300931008     C**** **** **** **** **** **** **** **** ****
256400931008     C****STAMPA TOT.GEN. :DETTAGLIO E TOTALE.
256500941012     C  N36              EXCEPT    DETTOT
256600931008     C**** **** **** **** **** **** **** **** ****
256700931008     C****PULIZIA CAMPI TOT.GEN.(DI TOTALE E DI DETTAGLIO)
256800931008     C*CAMPI DI TOTALE
256900931008     C                   Z-ADD     0             TKA123
257000970131     C                   Z-ADD     0             TGP88T
257100970131     C                   Z-ADD     0             TGP87T
257200970131     C                   Z-ADD     0             TGC88T
257300970131     C                   Z-ADD     0             TGC87T
257400931008     C                   Z-ADD     0             TKD123
257500931008     C                   Z-ADD     0             TKE123
257600931008     C                   Z-ADD     0             TKF123
257700931008     C                   Z-ADD     0             TGPETS
257800931008     C                   Z-ADD     0             TKI123
257900931008     C                   Z-ADD     0             TKL123
258000931008     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
258100931008     C*PER LA STAMPA).
258200931008     C                   MOVEA     *ZEROS        TKA(1)
258300931008     C                   MOVEA     *ZEROS        TKD(1)
258400931008     C                   MOVEA     *ZEROS        TKE(1)
258500931008     C                   MOVEA     *ZEROS        TKF(1)
258600931008     C                   MOVEA     *ZEROS        TKI(1)
258700931008     C                   MOVEA     *ZEROS        TKL(1)
258800940126      *
258900941012      * Verifico se vi sono fogli viaggio aperti
259000941012     C                   EXSR      FOGVIA
259100931130      *
259200931008     C                   ENDSR
259300941012      ****************************************************************
259400941012      * Scrivo in archivio il riepilogo per filiale (totali e c/serv.)
259500941012      ****************************************************************
259600941012     C     WRIFIL        BEGSR
259700941012      *
259800941012      * ---------------------- TOTALI DI FILIALE -------------------------------
259900941012      *
260000941012     C                   Z-ADD     SIMFEL        SPAFLE
260100941012     C                   Z-ADD     COMFIL        SPALNP
260200950210     C                   Z-ADD     DS1ADA        SPADRE
260300941012     C                   MOVE      *BLANK        SPATRE
260400941012      * ---- COLLI ----
260500970130     C                   Z-ADD     SKA(5)        SPACPE
260600970130     C                   Z-ADD     SKA(4)        SPACPC
260700970130     C                   Z-ADD     SKA(6)        SPACPD
260800970129     C                   Z-ADD     PB88F         SPA88B
260900970129     C                   Z-ADD     PI88          SPA88A
261000970129     C                   Z-ADD     PB87F         SPA87B
261100970129     C                   Z-ADD     PI87          SPA87A
261200941012      * ---- PESI ----
261300941012     C                   Z-ADD     SKD(5)        SPAPFE
261400970129     C                   Z-ADD     SKD(4)        SPAPFC
261500970129     C                   Z-ADD     SKD(6)        SPAPFD
261600970129     C                   Z-ADD     SKE(5)        SPAPOE
261700970129     C                   Z-ADD     SKE(4)        SPAPOC
261800970129     C                   Z-ADD     SKE(6)        SPAPOD
261900941012     C                   Z-ADD     PETS          SPAPTA
262000941012      * ---- VOLUMI ----
262100970130     C                   Z-ADD     SKI(5)        SPAVPE
262200970130     C                   Z-ADD     SKI(4)        SPAVPC
262300970130     C                   Z-ADD     SKI(6)        SPAVPD
262400941012      * ---- SPEDIZIONI --
262500970129     C                   Z-ADD     SKL(5)        SPASPE
262600970129     C                   Z-ADD     SKL(4)        SPASPC
262700970129     C                   Z-ADD     SKL(6)        SPASPD
262800970129     C                   Z-ADD     SKM(5)        SPASDE
262900970129     C                   Z-ADD     SKM(4)        SPASDC
263000970129     C                   Z-ADD     SKM(6)        SPASDD
263100970206     C                   Z-ADD     SKO(5)        SPASFE
263200970206     C                   Z-ADD     SKO(4)        SPASFC
263300970206     C                   Z-ADD     SKO(6)        SPASFD
263400970206     C                   Z-ADD     SKP(5)        SPATFE
263500970206     C                   Z-ADD     SKP(4)        SPATFC
263600970206     C                   Z-ADD     SKP(6)        SPATFD
263700941012      *
263800941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
263900941012      * riempo il "flag di trasmissione" (SPAFTR)
264000040921     C***  §5CSPA        IFEQ      'S'
264100040921     C***                MOVE      *BLANK        SPAFTR
264200040921     C***                MOVE      *ZEROS        SPADTR
264300040921     C***                ELSE
264400040921     C***                MOVE      'N'           SPAFTR
264500040921     C***                MOVE      *HIVAL        SPADTR
264600040921     C***                ENDIF
264700040921     c                   move      *blanks       spaftr
264800040921     c                   z-add     dateu         spadtr
264900970204     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI SCRIVO RECORD CON
265000970226     C* TIPO = 'F'
265100970226     C     WFES          IFEQ      'F'
265200970226     C     SPACPE        ANDEQ     *ZEROS
265300970204     C     SPACPC        ANDEQ     *ZEROS
265400970204     C     SPACPD        ANDEQ     *ZEROS
265500970204     C                   MOVE      'F'           SPATRE
265600970204     C                   END
265700970204     C                   WRITE     FNSPA000
265800941012      *
265900941012      * --------------------- CONTO SERVIZI DI FILIALE -------------------------
266000941012      *
266100941012      * Finche sono qua (totali di filiale), scrivo anche i c/serv. di filiale
266200941012     C                   Z-ADD     SIMFEL        SPAFLE
266300941012     C                   Z-ADD     COMFIL        SPALNP
266400950210     C                   Z-ADD     DS1ADA        SPADRE
266500941012     C                   MOVE      'C'           SPATRE
266600941012      * ---- COLLI ----
266700970130     C                   Z-ADD     SKA(8)        SPACPE
266800970130     C                   Z-ADD     SKA(7)        SPACPC
266900970130     C                   Z-ADD     SKA(9)        SPACPD
267000970130     C                   Z-ADD     PB88          SPA88B
267100970130     C                   Z-ADD     PS88          SPA88A
267200970130     C                   Z-ADD     PB87          SPA87B
267300970130     C                   Z-ADD     PS87          SPA87A
267400941012      * ---- PESI ----
267500941012     C                   Z-ADD     SKD(8)        SPAPFE
267600970130     C                   Z-ADD     SKD(7)        SPAPFC
267700970130     C                   Z-ADD     SKD(9)        SPAPFD
267800970130     C                   Z-ADD     SKE(8)        SPAPOE
267900970130     C                   Z-ADD     SKE(7)        SPAPOC
268000970130     C                   Z-ADD     SKE(9)        SPAPOD
268100941012     C                   Z-ADD     *ZERO         SPAPTA
268200941012      * ---- VOLUMI ----
268300970130     C                   Z-ADD     SKI(8)        SPAVPE
268400970130     C                   Z-ADD     SKI(7)        SPAVPC
268500970130     C                   Z-ADD     SKI(9)        SPAVPD
268600941012      * ---- SPEDIZIONI --
268700970130     C                   Z-ADD     SKL(8)        SPASPE
268800970130     C                   Z-ADD     SKL(7)        SPASPC
268900970130     C                   Z-ADD     SKL(9)        SPASPD
269000970130     C                   Z-ADD     SKM(8)        SPASDE
269100970130     C                   Z-ADD     SKM(7)        SPASDC
269200970130     C                   Z-ADD     SKM(9)        SPASDD
269300970206     C                   Z-ADD     SKO(8)        SPASFE
269400970206     C                   Z-ADD     SKO(7)        SPASFC
269500970206     C                   Z-ADD     SKO(9)        SPASFD
269600970206     C                   Z-ADD     SKP(8)        SPATFE
269700970206     C                   Z-ADD     SKP(7)        SPATFC
269800970206     C                   Z-ADD     SKP(9)        SPATFD
269900941012      *
270000941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
270100941012      * riempo il "flag di trasmissione" (SPAFTR)
270200040921     C***  §5CSPA        IFEQ      'S'
270300040921     C***                MOVE      *BLANK        SPAFTR
270400040921     C***                MOVE      *ZEROS        SPADTR
270500040921     C***                ELSE
270600040921     C***                MOVE      'N'           SPAFTR
270700040921     C***                MOVE      *HIVAL        SPADTR
270800040921     C***                ENDIF
270900040921     c                   clear                   spaftr
271000040921     c                   move      dateu         spadtr
271100941012      *
271200970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI NON SCRIVO NIENTE
271300970226     C     WFES          IFEQ      'F'
271400970226     C     SPACPE        ANDEQ     *ZEROS
271500970226     C     SPACPC        ANDEQ     *ZEROS
271600970226     C     SPACPD        ANDEQ     *ZEROS
271700970226     C                   ELSE
271800970226     C                   WRITE     FNSPA000
271900970226     C                   END
272000941012      *
272100941012     C                   ENDSR
272200941012      ****************************************************************
272300941012      * Scrivo in archivio il riepilogo per transiti
272400941012      ****************************************************************
272500941012     C     WRITRA        BEGSR
272600941012      *
272700941012     C                   Z-ADD     SIMFEL        SPAFLE
272800941012     C                   Z-ADD     SIMFEL        SPALNP
272900950210     C                   Z-ADD     DS1ADA        SPADRE
273000941012     C                   MOVE      'T'           SPATRE
273100941012      * ---- COLLI ----
273200970130     C                   Z-ADD     XKA(2)        SPACPE
273300970130     C                   Z-ADD     XKA(1)        SPACPC
273400970130     C                   Z-ADD     XKA(3)        SPACPD
273500970130     C* N.B.:Nei transiti non si distinguono i colli disk B dagli altri
273600970130     C                   MOVE      *ZEROS        SPA88B
273700970130     C                   Z-ADD     TRPI88        SPA88A
273800970130     C                   MOVE      *ZEROS        SPA87B
273900970130     C                   Z-ADD     TRPI87        SPA87A
274000941012      * ---- PESI ----
274100941012     C                   Z-ADD     XKD(2)        SPAPFE
274200970203     C                   Z-ADD     XKD(1)        SPAPFC
274300970203     C                   Z-ADD     XKD(3)        SPAPFD
274400970130     C                   Z-ADD     XKE(2)        SPAPOE
274500970130     C                   Z-ADD     XKE(1)        SPAPOC
274600970130     C                   Z-ADD     XKE(3)        SPAPOD
274700941012     C                   Z-ADD     TRPETS        SPAPTA
274800941012      * ---- VOLUMI ----
274900970130     C                   Z-ADD     XKI(2)        SPAVPE
275000970130     C                   Z-ADD     XKI(1)        SPAVPC
275100970130     C                   Z-ADD     XKI(3)        SPAVPD
275200941012      * ---- SPEDIZIONI --
275300970130     C                   Z-ADD     XKL(2)        SPASPE
275400970130     C                   Z-ADD     XKL(1)        SPASPC
275500970130     C                   Z-ADD     XKL(3)        SPASPD
275600970130     C                   Z-ADD     *ZERO         SPASDE
275700970130     C                   Z-ADD     *ZERO         SPASDC
275800970130     C                   Z-ADD     *ZERO         SPASDD
275900970130     C                   Z-ADD     *ZERO         SPASFE
276000970130     C                   Z-ADD     *ZERO         SPASFC
276100970130     C                   Z-ADD     *ZERO         SPASFD
276200941012      *
276300941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
276400941012      * riempo il "flag di trasmissione" (SPAFTR)
276500040921     C***  §5CSPA        IFEQ      'S'
276600040921     C***                MOVE      *BLANKS       SPAFTR
276700040921     C***                MOVE      *ZEROS        SPADTR
276800040921     C***                ELSE
276900040921     C***                MOVE      'N'           SPAFTR
277000040921     C***                MOVE      *HIVAL        SPADTR
277100040921     C***                ENDIF
277200040921     c                   clear                   spaftr
277300040921     c                   z-add     dateu         spadtr
277400941012      *
277500970130     C     SPACPE        IFGT      *ZERO
277600970130     C     SPACPC        ORGT      *ZERO
277700970130     C     SPACPD        ORGT      *ZERO
277800970130     C                   WRITE     FNSPA000
277900941012     C                   ENDIF
278000941012      *
278100941012     C                   ENDSR
278200941012      ****************************************************************
278300970130      * Routine di RISTAMPA: viene semplicemente letto il file FNSPA00F
278400941012      * e stampato, senza alcuna elaborazione di dati.
278500941012      * E' quindi possibile ristampare piu' giorni accorpandoli insieme.
278600941012      * Le routine di RISTAMPA sono simili a quelle del pgm FNLSC2R.
278700941012      ****************************************************************
278800941012     C     RISTAM        BEGSR
278900050829     c                   setoff                                       3534
279000941012      *
279100941012      * Verifico se richiesta una specifica area
279200950210     C     DS1FEL        COMP      *ZERO                                  34
279300050829     c
279400050829     c* Se sono in filiale imposto = simfel
279500050829     c                   if        ds1fel=0 and simfel>0
279600050829     c                   eval      ds1fel=simfel
279700050829     c                   setoff                                       34
279800050829     c                   endif
279900050829     c
280000050829     c* Se devo elaborare una sola linea di partenza, leggo con
280100050829     c*  la linea
280200050829     c                   if        lin(2)=0 and lin(1)>0
280300050829     c                   eval      klin=lin(1)
280400050829     c                   eval      kdre=ds1ada
280500050829     c                   seton                                        35
280600050829     c                   endif
280700050829     c
280800020813     C                   MOVE      *ZEROS        XD
280900941012      *
281000941012     C                   Z-ADD     *ZERO         $PRIM1            1 0
281100941012     C                   Z-ADD     *ZERO         $FILPR            3 0
281200941012     C                   Z-ADD     *ZERO         $FI2PR            3 0
281300941012      *
281400050829     C  n35DS1FEL        SETLL     FNSPA01L
281500050829     C   35kspa          SETLL     FNSPA01L
281600941012     C                   DO        *HIVAL
281700050829     c* 35 on - lettura con lnp
281800050829     c                   if        *in35
281900050829     C     kspa2         READE(N)  FNSPA01L                               33
282000050829     c                   else
282100970130     C  N34DS1FEL        READE(N)  FNSPA01L                               33
282200970130     C   34              READ(N)   FNSPA01L                               33
282300050829     c                   endif
282400941012      *
282500970130     C  N33SPADRE        IFGE      DS1ADA
282600970130     C     SPADRE        ANDLE     DS1AAL
282700941012      *
282800941012      * La linea deve essere presente in LIN
282900050829     c                   if        not *in35 and *in34
283000941012     C     SPALNP        LOOKUP    LIN                                    32
283100050829     c                   else
283200050829     c                   seton                                        32
283300050829     c                   endif
283400941012      *
283500941012     C     *IN32         IFEQ      *ON
283600941012      *
283700941012     C     SPATRE        IFEQ      *BLANK
283800941012      *
283900941012      * Al primo giro memorizzo il "precedente"
284000941012     C     $PRIM1        IFEQ      *ZERO
284100941012     C                   Z-ADD     SPAFLE        $FI2PR
284200941012     C                   Z-ADD     SPALNP        $FILPR
284300941012     C                   Z-ADD     1             $PRIM1
284400941012     C                   ENDIF
284500941012      *
284600941012      * Se cambia filiale stampo i totali per filiale
284700941012     C     SPALNP        IFNE      $FILPR
284800020813     C                   ADD       1             XD
284900941012     C                   Z-ADD     $FILPR        COMFIL
285000941012     C                   MOVE      *BLANKS       DESLNP
285100941012     C     $FILPR        CHAIN     AZORG01L                           99
285200970203     C     *IN99         IFEQ      *OFF
285300970203     C     ORGDE5        IFNE      *BLANKS
285400970203     C                   MOVEL     ORGDE5        DESLNP
285500970203     C                   ELSE
285600970203     C                   MOVEL     ORGDES        DESLNP
285700970203     C                   END
285800970203     C                   END
285900941012     C                   EXSR      TOTLNP
286000941012     C                   Z-ADD     SPALNP        $FILPR
286100941012     C                   ENDIF
286200941012      *
286300941012      * Se cambia area stampo i totali per c/servizio
286400941012     C     SPAFLE        IFNE      $FI2PR
286500941012     C                   EXSR      TOTCS
286600941012     C                   EXSR      TOTARE
286700000308     C  N08              XFOOT     XKA           TOTXKA            7 0
286800000308     C     *IN08         IFEQ      *ON
286900000308     C     TOTXKA        ORGT      0
287000000308     C                   EXSR      TOTTRA
287100000308     C                   ENDIF
287200941012     C                   Z-ADD     SPAFLE        $FI2PR
287300941012     C                   ENDIF
287400000308      *
287500941012     C                   ENDIF
287600941012      *
287700941012      * Aggiorno i totali
287800941012     C                   EXSR      SERTRA
287900941012      *
288000941012     C                   ENDIF
288100941012     C                   ENDIF
288200941012      *
288300941012     C  N33              ENDDO
288400941012      *
288500941012      * Stampo l'ultimo totale filiale ed il c/servizio
288600941012     C                   Z-ADD     $FILPR        COMFIL
288700941012     C                   MOVE      *BLANKS       DESLNP
288800941012     C     $FILPR        CHAIN     AZORG01L                           99
288900970203     C     *IN99         IFEQ      *OFF
289000970203     C     ORGDE5        IFNE      *BLANKS
289100970203     C                   MOVEL     ORGDE5        DESLNP
289200970203     C                   ELSE
289300970203     C                   MOVEL     ORGDES        DESLNP
289400970203     C                   END
289500970203     C                   END
289600941012     C                   EXSR      TOTLNP
289700941012     C                   EXSR      TOTCS
289800941012     C                   EXSR      TOTARE
289900941012      *
290000941012      * Stampo il transito
290100000308     C  N08              XFOOT     XKA           TOTXKA            7 0
290200000308     C     *IN08         IFEQ      *ON
290300000308     C     TOTXKA        ORGT      0
290400000308     C                   EXSR      TOTTRA
290500000308     C                   ENDIF
290600941012      *
290700941012     C     ENDRIS        ENDSR
290800941012      *****************************************************************
290900941012      * RISTAMPA: mantengo aggiornati i totali
291000941012      *****************************************************************
291100941012     C     SERTRA        BEGSR
291200941012      *
291300941012     C     SPATRE        IFEQ      *BLANK
291400941012      * Colli
291500970130     C                   ADD       SPACPC        SKA(4)
291600970130     C                   ADD       SPACPE        SKA(5)
291700970130     C                   ADD       SPACPD        SKA(6)
291800970130     C                   ADD       SPA88B        PB88F
291900970130     C                   ADD       SPA88A        PI88
292000970130     C                   ADD       SPA87B        PB87F
292100970130     C                   ADD       SPA87A        PI87
292200941012      * Pesi
292300970130     C                   ADD       SPAPFC        SKD(4)
292400970130     C                   ADD       SPAPFE        SKD(5)
292500970130     C                   ADD       SPAPFD        SKD(6)
292600970130     C                   ADD       SPAPOC        SKE(4)
292700970130     C                   ADD       SPAPOE        SKE(5)
292800970130     C                   ADD       SPAPOD        SKE(6)
292900970130     C     SKD(4)        ADD       SKE(4)        SKF(4)
293000970130     C     SKD(5)        ADD       SKE(5)        SKF(5)
293100970130     C     SKD(6)        ADD       SKE(6)        SKF(6)
293200941012     C                   ADD       SPAPTA        PETS
293300941012      * Volumi
293400970130     C                   ADD       SPAVPC        SKI(4)
293500970130     C                   ADD       SPAVPE        SKI(5)
293600970130     C                   ADD       SPAVPD        SKI(6)
293700941012      * Spedizioni
293800970130     C                   ADD       SPASPC        SKL(4)
293900970130     C                   ADD       SPASPE        SKL(5)
294000970130     C                   ADD       SPASPD        SKL(6)
294100970130     C                   ADD       SPASDC        SKM(4)
294200970130     C                   ADD       SPASDE        SKM(5)
294300970130     C                   ADD       SPASDD        SKM(6)
294400970206     C     SPASFC        ADD       SPATFC        SKN(4)
294500970206     C     SPASFE        ADD       SPATFE        SKN(5)
294600970206     C     SPASFD        ADD       SPATFD        SKN(6)
294700941012     C                   ENDIF
294800941012      *
294900941012     C     SPATRE        IFEQ      'C'
295000941012      * Colli
295100970130     C                   ADD       SPACPC        SKA(1)
295200970130     C                   ADD       SPACPE        SKA(2)
295300970130     C                   ADD       SPACPD        SKA(3)
295400970130     C                   ADD       SPA88B        PB88CS
295500970130     C                   ADD       SPA88B        PB88
295600970130     C                   ADD       SPA88A        PI88CS
295700970130     C                   ADD       SPA88A        PS88
295800970130     C                   ADD       SPA87B        PB87CS
295900970130     C                   ADD       SPA87B        PB87
296000970130     C                   ADD       SPA87A        PI87CS
296100970130     C                   ADD       SPA87A        PS87
296200941012      * Pesi
296300970130     C                   ADD       SPAPFC        SKD(1)
296400970130     C                   ADD       SPAPFE        SKD(2)
296500970130     C                   ADD       SPAPFD        SKD(3)
296600970130     C                   ADD       SPAPOC        SKE(1)
296700970130     C                   ADD       SPAPOE        SKE(2)
296800970130     C                   ADD       SPAPOD        SKE(3)
296900970130     C     SKD(1)        ADD       SKE(1)        SKF(1)
297000970130     C     SKD(2)        ADD       SKE(2)        SKF(2)
297100970130     C     SKD(3)        ADD       SKE(3)        SKF(3)
297200941012      * Volumi
297300970130     C                   ADD       SPAVPC        SKI(1)
297400970130     C                   ADD       SPAVPE        SKI(2)
297500970130     C                   ADD       SPAVPD        SKI(3)
297600941012      * Spedizioni
297700970130     C                   ADD       SPASPC        SKL(1)
297800970130     C                   ADD       SPASPE        SKL(2)
297900970130     C                   ADD       SPASPD        SKL(3)
298000970130     C                   ADD       SPASDC        SKM(1)
298100970130     C                   ADD       SPASDE        SKM(2)
298200970130     C                   ADD       SPASDD        SKM(3)
298300970228     C     SPATFC        ADD       SPASFC        W0010
298400970228     C     SPATFE        ADD       SPASFE        W0020
298500970228     C     SPATFD        ADD       SPASFD        W0030
298600970228     C                   ADD       W0010         SKN(1)
298700970228     C                   ADD       W0020         SKN(2)
298800970228     C                   ADD       W0030         SKN(3)
298900941012     C                   ENDIF
299000941012      *
299100941012     C     SPATRE        IFEQ      'T'
299200970130     C                   ADD       SPACPC        XKA(1)
299300970130     C                   ADD       SPACPE        XKA(2)
299400970130     C                   ADD       SPACPD        XKA(3)
299500970130     C                   ADD       SPA88A        TRPI88
299600970130     C                   ADD       SPA87A        TRPI87
299700941012      * Pesi
299800970130     C                   ADD       SPAPFC        XKD(1)
299900970130     C                   ADD       SPAPFE        XKD(2)
300000970130     C                   ADD       SPAPFD        XKD(3)
300100970130     C                   ADD       SPAPOC        XKE(1)
300200970130     C                   ADD       SPAPOE        XKE(2)
300300970130     C                   ADD       SPAPOD        XKE(3)
300400970228     C     XKD(1)        ADD       XKE(1)        XKF(1)
300500970228     C     XKD(2)        ADD       XKE(2)        XKF(2)
300600970228     C     XKD(3)        ADD       XKE(3)        XKF(3)
300700941012     C                   ADD       SPAPTA        TRPETS
300800941012      * Volumi
300900970130     C                   ADD       SPAVPC        XKI(1)
301000970130     C                   ADD       SPAVPE        XKI(2)
301100970130     C                   ADD       SPAVPD        XKI(3)
301200941012      * Spedizioni
301300970130     C                   ADD       SPASPC        XKL(1)
301400970130     C                   ADD       SPASPE        XKL(2)
301500970130     C                   ADD       SPASPD        XKL(3)
301600941012     C                   ENDIF
301700941012      *
301800941012     C                   ENDSR
301900941012      ****************************************************************
302000941012      * Verifica se vi sono dei fogli viaggio con data partenza compresa
302100941012      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
302200941012      ****************************************************************
302300941012     C     FOGVIA        BEGSR
302400941012      *
302500950404     C  N36DS1RIS        IFEQ      'S'
302600941012      *
302700020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
302800020813     ***C     *LOVAL        SETLL     FNFGV04L
302900020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
303000020813     C     SIMfel        setll     FNFGV06L
303100941012     C                   DO        *HIVAL
303200020813     ***C                   READ      FNFGV04L                               31
303300020813     C     SIMfel        reade     FNFGV06L                               31
303400950210     C  N31FGVDTP        IFGE      DS1ADA
303500950210     C     FGVDTP        ANDLE     DS1AAL
303600941012     C                   SETON                                        31
303700941012     C                   EXCEPT    FVAPER
303800941012     C                   ENDIF
303900941012     C  N31              ENDDO
304000941012      *
304100941012     C                   ENDIF
304200941012      *
304300941012     C                   ENDSR
304400940103      ****************************************************************
304500941012      * ----------------?    TESTATA  DI  STAMPA    ?-----------------
304600930928     O*PRIMA RIGA SE C'E' SOLO 'DATA DA'
304700930928     OPRTF198   E   N03      TESTA            02
304800930928     O                       SK(1)               66
304900930928     O                       SK(2)              132
305000930928     O                       SK(4)              198
305100930928     O                       RSUT                20
305200941012     O                       STRIP               48
305300950210     O                       DS1GDA             113 '  /  /    '
305400950210     O                       WDATE              181 '  /  /    '
305500930928     O                       PAGE          Z    198
305600930928     O*PRIMA RIGA SE C'E' 'DATA DA' E 'DATA A'
305700930928     O          E    03      TESTA            02
305800930928     O                       SK(1)               66
305900930928     O                       SK(3)              132
306000930928     O                       SK(4)              198
306100930928     O                       RSUT                20
306200930928     O                       SIMFEL              24
306300941012     O                       STRIP               48
306400950210     O                       DS1GDA             113 '  /  /    '
306500950210     O                       DS1GAL             128 '  /  /    '
306600950210     O                       WDATE              181 '  /  /    '
306700930928     O                       PAGE          Z    198
306800930928     O*RIGA IN CUI C'E' SOLO UTIME
306900930928     O          E            TESTA            03
307000950111     O                       KNSIF               10
307100950111     O                       KNMUS               21
307200941012     O                       STRIP1              48
307300950210     O                       UTIME              180 '  :  :  '
307400930928     O*TRATTINI
307500930928     O          E            TESTA            04
307600930928     O                       SK(5)               66
307700930928     O                       SK(5)              132
307800970306     O                       SK(18)             198
307900930928     O*PRIMA RIGA DI COSTANTI
308000930928     O          E            TESTA            05
308100930928     O                       SK(6)               66
308200970131     O                       SK(9)              132
308300970131     O                       SK(12)             198
308400930928     O*SECONDA RIGA DI COSTANTI
308500930928     O          E            TESTA            06
308600970131     O                       SK(7)               66
308700930928     O                       SK(10)             132
308800970131     O                       SK(13)             198
308900930928     O*TERZA RIGA DI COSTANTI
309000930928     O          E            TESTA            07
309100970131     O                       SK(8)               66
309200970131     O                       SK(11)             132
309300930928     O                       SK(14)             198
309400930928     O*TRATTINI
309500970131     O          E            TESTA            08
309600930928     O                       SK(5)               66
309700930928     O                       SK(5)              132
309800970306     O                       SK(18)             198
309900941012      * ----------------?    DETTAGLIO DI FILIALE    ?----------------
310000930928     O****1°RIGA DEL DETTAGLIO FILIALE = TOTALI
310100930928     O**COLLI
310200930929     O          E            DETFIL      2
310300970131     O                       DESLNP              10
310400970131     O                       SKA456        Z     18
310500970131     O*COLLI NO SPUNT. IN PART. DISK B
310600970131     O                       PB88F         Z     26
310700970131     O                       PC88B         4     33
310800970131     O                       PER3B               34
310900970131     O*COLLI NO SPUNT. IN PART. ALTRI
311000970131     O                       PI88          Z     42
311100970131     O                       PC88          4     49
311200970131     O                       PER3                50
311300970131     O*COLLI NO SPUNT. IN PART. TOTALE
311400970131     O                       PI88T         Z     58
311500970131     O                       PC88T         4     65
311600970131     O                       PER3T               66
311700970131     O*COLLI PISTOLA 87 (EX 88) DISK B
311800970131     O                       PB87F         Z     74
311900970131     O                       PC87B         4     81
312000970131     O                       PER34B              82
312100970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
312200970131     O                       PI87          Z     90
312300970131     O                       PC87          4     97
312400970131     O                       PER34               98
312500970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
312600970131     O                       PI87T         Z    106
312700970131     O                       PC87T         4    113
312800970131     O                       PER34T             114
312900930928     O**PESO
313000970131     O                       SKD456        4    124
313100970131     O                       SKE456        4    134
313200970131     O                       SKF456        4    144
313300930928     O**PESO TASSABILE
313400970131     O                       PETS          4    155
313500930928     O**VOLUME
313600970131     O                       SKI456        4    165
313700930928     O**SPEDIZIONI
313800970131     O                       SKL456        Z    174
313900970131     O                       SKM456        Z    182
314000930928     O*
314100931011     O****2°RIGA DEL DETTAGLIO FILIALE = ESPRESSO
314200930928     O**COLLI
314300930929     O          E            DETFIL      1
314400970131     O                                           11 'ESPR'
314500970131     O                       SKA(5)        Z     18
314600930928     O**PESO
314700970131     O                       SKD(5)        4    124
314800970131     O                       SKE(5)        4    134
314900970131     O                       SKF(5)        4    144
315000930928     O**VOLUME
315100970131     O                       SKI(5)        4    165
315200930928     O**SPEDIZIONI
315300970131     O                       SKL(5)        Z    174
315400970131     O                       SKM(5)        Z    182
315500931011     O*
315600950111     O****3°RIGA DEL DETTAGLIO FILIALE = DISTRIBUZIONE
315700930928     O**COLLI
315800930929     O          E            DETFIL      1
315900970131     O                                           11 'DIST'
316000970131     O                       SKA(6)        Z     18
316100930928     O**PESO
316200970131     O                       SKD(6)        4    124
316300970131     O                       SKE(6)        4    134
316400970131     O                       SKF(6)        4    144
316500930928     O**VOLUME
316600970131     O                       SKI(6)        4    165
316700930928     O**SPEDIZIONI
316800970131     O                       SKL(6)        Z    174
316900970131     O                       SKM(6)        Z    182
317000940323     O*
317100950111     O****4°RIGA DEL DETTAGLIO FILIALE = CORRIERE
317200940323     O**COLLI
317300940323     O          E            DETFIL      1
317400970131     O                                           11 'CORR'
317500970131     O                       SKA(4)        Z     18
317600940323     O**PESO
317700970131     O                       SKD(4)        4    124
317800970131     O                       SKE(4)        4    134
317900970131     O                       SKF(4)        4    144
318000940323     O**VOLUME
318100970131     O                       SKI(4)        4    165
318200940323     O**SPEDIZIONI
318300970131     O                       SKL(4)        Z    174
318400970131     O                       SKM(4)        Z    182
318500941012      * ----------?    RIEPILOGO C/SERVIZI PER FILIALE    ?-----------
318600930929     O****1°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = TOTALI
318700930929     O**COLLI
318800970131     O          E            DETCS       2
318900970131     O                                           10 'C/SERVIZI '
319000970131     O                       SKA123        Z     18
319100970131     O*COLLI NO SPUNT. IN PART. DISK B
319200970131     O                       PB88CS        Z     26
319300970131     O                       PC88CB        4     33
319400970131     O                       PER3B               34
319500970131     O*COLLI NO SPUNT. IN PART. ALTRI
319600970131     O                       PI88CS        Z     42
319700970131     O                       PC88CS        4     49
319800970131     O                       PER3                50
319900970131     O*COLLI NO SPUNT. IN PART. TOTALE
320000970131     O                       P88CST        Z     58
320100970131     O                       PC88CT        4     65
320200970131     O                       PER3T               66
320300970131     O*COLLI PISTOLA 87 (EX 88) DISKB
320400970131     O                       PB87CS        Z     74
320500970131     O                       PC87CB        4     81
320600970131     O                       PER34B              82
320700970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
320800970131     O                       PI87CS        Z     90
320900970131     O                       PC87CS        4     97
321000970131     O                       PER34               98
321100970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
321200970131     O                       P87CST        Z    106
321300970131     O                       PC87CT        4    113
321400970131     O                       PER34T             114
321500930929     O**PESO
321600970131     O                       SKD123        4    124
321700970131     O                       SKE123        4    134
321800970131     O                       SKF123        4    144
321900930929     O**PESO TASSABILE (PER C/SERVIZI NON C'E').
322000930929     O**VOLUME
322100970131     O                       SKI123        4    165
322200930929     O**SPEDIZIONI
322300970131     O                       SKL123        Z    174
322400970131     O                       SKM123        Z    182
322500930929     O*
322600931011     O****2°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = ESPRESSO
322700930929     O**COLLI
322800930929     O          E            DETCS       1
322900970131     O                                           11 'ESPR'
323000970131     O                       SKA(2)        Z     18
323100930929     O**PESO
323200970131     O                       SKD(2)        4    124
323300970131     O                       SKE(2)        4    134
323400970131     O                       SKF(2)        4    144
323500930929     O**VOLUME
323600970131     O                       SKI(2)        4    165
323700930929     O**SPEDIZIONI
323800970131     O                       SKL(2)        Z    174
323900970131     O                       SKM(2)        Z    182
324000931011     O*
324100950111     O****3°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = DISTRIBUZIONE
324200930929     O**COLLI
324300930929     O          E            DETCS       1
324400970131     O                                           11 'DIST'
324500970131     O                       SKA(3)        Z     18
324600930929     O**PESO
324700970131     O                       SKD(3)        4    124
324800970131     O                       SKE(3)        4    134
324900970131     O                       SKF(3)        4    144
325000930929     O**VOLUME
325100970131     O                       SKI(3)        4    165
325200930929     O**SPEDIZIONI
325300970131     O                       SKL(3)        Z    174
325400970131     O                       SKM(3)        Z    182
325500940323     O*
325600950111     O****4°RIGA DEL DETTAGLIO FILIALE C/SERVIZI= CORRIERE
325700940323     O**COLLI
325800940323     O          E            DETCS       1
325900970131     O                                           11 'CORR'
326000970131     O                       SKA(1)        Z     18
326100940323     O**PESO
326200970131     O                       SKD(1)        4    124
326300970131     O                       SKE(1)        4    134
326400970131     O                       SKF(1)        4    144
326500940323     O**VOLUME
326600970131     O                       SKI(1)        4    165
326700940323     O**SPEDIZIONI
326800970131     O                       SKL(1)        Z    174
326900970131     O                       SKM(1)        Z    182
327000941012      * ----------------?    RIEPILOGO PER AREA    ?------------------
327100941012     O*TRATTINI DI SEPARAZIONE TRA I  C/SERVIZI E L'AREA
327200941012     O          E            DETARE      1
327300941012     O                       SK(5)               66
327400941012     O                       SK(5)              132
327500970306     O                       SK(18)             198
327600930930     O****1°RIGA DEL DETTAGLIO AREA = TOTALI
327700930930     O**COLLI
327800931011     O          E            DETARE      1
327900970131     O                                           10 'TOTALE    '
328000970131     O                       AKA123        Z     18
328100970131     O*COLLI NO SPUNT. IN PART. DISK B
328200970131     O                       ARPB88        Z     26
328300970131     O                       APC88B        4     33
328400970131     O                       PER3B               34
328500970131     O*COLLI NO SPUNT. IN PART. ALTRI
328600970131     O                       ARPI88        Z     42
328700970131     O                       ARPC88        4     49
328800970131     O                       PER3                50
328900970131     O*COLLI NO SPUNT. IN PART. TOTALE
329000970131     O                       ARP88T        Z     58
329100970131     O                       APC88T        4     65
329200970131     O                       PER3T               66
329300970131     O*COLLI PISTOLA 87 (EX 88) DISK B
329400970131     O                       ARPB87        Z     74
329500970131     O                       APC87B        4     81
329600970131     O                       PER34B              82
329700970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
329800970131     O                       ARPI87        Z     90
329900970131     O                       ARPC87        4     97
330000970131     O                       PER34               98
330100970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
330200970131     O                       ARP87T        Z    106
330300970131     O                       APC87T        4    113
330400970131     O                       PER34T             114
330500930930     O**PESO
330600970131     O                       AKD123        4    124
330700970131     O                       AKE123        4    134
330800970131     O                       AKF123        4    144
330900930930     O**PESO TASSABILE
331000970131     O                       ARPETS        4    155
331100930930     O**VOLUME
331200970131     O                       AKI123        4    165
331300930930     O**SPEDIZIONI
331400970131     O                       AKL123        Z    174
331500970131     O                       AKM123        Z    182
331600930930     O*
331700931011     O****2°RIGA DEL DETTAGLIO AREA = ESPRESSO
331800930930     O**COLLI
331900930930     O          E            DETARE      1
332000970131     O                                           11 'ESPR'
332100970131     O                       AKA(2)        Z     18
332200930930     O**PESO
332300970131     O                       AKD(2)        4    124
332400970131     O                       AKE(2)        4    134
332500970131     O                       AKF(2)        4    144
332600930930     O**VOLUME
332700970131     O                       AKI(2)        4    165
332800930930     O**SPEDIZIONI
332900970131     O                       AKL(2)        Z    174
333000970131     O                       AKM(2)        Z    182
333100931011     O*
333200950111     O****3°RIGA DEL DETTAGLIO AREA = DISTRIBUZIONE
333300930930     O**COLLI
333400940324     O          E            DETARE      1
333500970131     O                                           11 'DIST'
333600970131     O                       AKA(3)        Z     18
333700930930     O**PESO
333800970131     O                       AKD(3)        4    124
333900970131     O                       AKE(3)        4    134
334000970131     O                       AKF(3)        4    144
334100930930     O**VOLUME
334200970131     O                       AKI(3)        4    165
334300930930     O**SPEDIZIONI
334400970131     O                       AKL(3)        Z    174
334500970131     O                       AKM(3)        Z    182
334600940323     O*
334700950111     O****4°RIGA DEL DETTAGLIO AREA = CORRIERE
334800940323     O**COLLI
334900940324     O          E            DETARE      1  1
335000970203     O                                           11 'CORR'
335100970203     O                       AKA(1)        Z     18
335200940323     O**PESO
335300970203     O                       AKD(1)        4    124
335400970203     O                       AKE(1)        4    134
335500970203     O                       AKF(1)        4    144
335600940323     O**VOLUME
335700970203     O                       AKI(1)        4    165
335800940323     O**SPEDIZIONI
335900970203     O                       AKL(1)        Z    174
336000970203     O                       AKM(1)        Z    182
336100941012      * ----------------?    RIEPILOGO TRANSITI    ?------------------
336200931008     O****1°RIGA DEL DETTAGLIO TRANSITI = TOTALI
336300931008     O**COLLI
336400970203     O          E            DETTRA      1
336500970203     O                                           10 'TRANSITI  '
336600970203     O                       XKA123        Z     18
336700931008     O*COLLI NO SPUNT. IN PART.
336800970203     O                       TRPI88        Z     58
336900970203     O                       TRPC88        4     65
337000970203     O                       PER3                66
337100950622     O*COLLI PISTOLA 87 (EX 88 E 89)
337200970203     O                       TRPI87        Z    106
337300970203     O                       TRPC87        4    113
337400970203     O                       PER34              114
337500931008     O**PESO
337600970203     O                       XKD123        4    124
337700970203     O                       XKE123        4    134
337800970203     O                       XKF123        4    144
337900931008     O**PESO TASSABILE
338000970203     O                       TRPETS        4    155
338100931008     O**VOLUME
338200970203     O                       XKI123        4    165
338300931008     O**SPEDIZIONI
338400970203     O                       XKL123        Z    174
338500931008     O*
338600931011     O****2°RIGA DEL DETTAGLIO TRANSITI = ESPRESSO
338700931008     O**COLLI
338800931008     O          E            DETTRA      1
338900970203     O                                           11 'ESPR'
339000970203     O                       XKA(2)        Z     18
339100931008     O**PESO
339200970203     O                       XKD(2)        4    124
339300970203     O                       XKE(2)        4    134
339400970203     O                       XKF(2)        4    144
339500931008     O**VOLUME
339600970203     O                       XKI(2)        4    165
339700931008     O**SPEDIZIONI
339800970203     O                       XKL(2)        Z    174
339900931011     O*
340000950111     O****3°RIGA DEL DETTAGLIO TRANSITI = DISTRIBUZIONE
340100931008     O**COLLI
340200931008     O          E            DETTRA      1
340300970203     O                                           11 'DIST'
340400970203     O                       XKA(3)        Z     18
340500931008     O**PESO
340600970203     O                       XKD(3)        4    124
340700970203     O                       XKE(3)        4    134
340800970203     O                       XKF(3)        4    144
340900931008     O**VOLUME
341000970203     O                       XKI(3)        4    165
341100931008     O**SPEDIZIONI
341200970203     O                       XKL(3)        Z    174
341300940323     O*
341400950111     O****4°RIGA DEL DETTAGLIO TRANSITI = CORRIERE
341500940323     O**COLLI
341600940323     O          E            DETTRA      1
341700970203     O                                           11 'CORR'
341800970203     O                       XKA(1)        Z     18
341900940323     O**PESO
342000970203     O                       XKD(1)        4    124
342100970203     O                       XKE(1)        4    134
342200970203     O                       XKF(1)        4    144
342300940323     O**VOLUME
342400970203     O                       XKI(1)        4    165
342500940323     O**SPEDIZIONI
342600970203     O                       XKL(1)        Z    174
342700941012      * ------------------?    TOTALI  GENERALI    ?------------------
342800940401     O*TRATTINI DI SEPARAZIONE TRA I TRANSITI ED IL TOTALE GENERALE.
342900941012     O          E            DETTOT      1
343000941012     O                       SK(5)               66
343100941012     O                       SK(5)              132
343200970306     O                       SK(18)             198
343300931008     O****1°RIGA DEL DETTAGLIO TOT.GEN. = TOTALI
343400931008     O**COLLI
343500970203     O          E            DETTOT      1
343600970203     O                                           10 'TOT.GENER.'
343700970203     O                       TKA123        Z     18
343800931008     O*COLLI NO SPUNT. IN PART.
343900970203     O                       TGP88T        Z     58
344000970203     O                       TGC88T        4     65
344100970203     O                       PER3                66
344200950622     O*COLLI PISTOLA 87 (EX 88 E 89)
344300970203     O                       TGP87T        Z    106
344400970203     O                       TGC87T        4    113
344500970203     O                       PER34              114
344600931008     O**PESO
344700970203     O                       TKD123        4    124
344800970203     O                       TKE123        4    134
344900970203     O                       TKF123        4    144
345000931008     O**PESO TASSABILE
345100970203     O                       TGPETS        4    155
345200931008     O**VOLUME
345300970203     O                       TKI123        4    165
345400931008     O**SPEDIZIONI
345500970203     O                       TKL123        Z    174
345600931008     O*
345700931011     O****2°RIGA DEL DETTAGLIO TOT.GEN. = ESPRESSO
345800931008     O**COLLI
345900931008     O          E            DETTOT      1
346000970203     O                                           11 'ESPR'
346100970203     O                       TKA(2)        Z     18
346200931008     O**PESO
346300970203     O                       TKD(2)        4    124
346400970203     O                       TKE(2)        4    134
346500970203     O                       TKF(2)        4    144
346600931008     O**VOLUME
346700970203     O                       TKI(2)        4    165
346800931008     O**SPEDIZIONI
346900970203     O                       TKL(2)        Z    174
347000931008     O*
347100950111     O****3°RIGA DEL DETTAGLIO TOT.GEN. = DISTRIBUZIONE
347200931008     O**COLLI
347300931008     O          E            DETTOT      1
347400970203     O                                           11 'DIST'
347500970203     O                       TKA(3)        Z     18
347600931008     O**PESO
347700970203     O                       TKD(3)        4    124
347800970203     O                       TKE(3)        4    134
347900970203     O                       TKF(3)        4    144
348000931008     O**VOLUME
348100970203     O                       TKI(3)        4    165
348200931008     O**SPEDIZIONI
348300970203     O                       TKL(3)        Z    174
348400940323     O*
348500950111     O****4°RIGA DEL DETTAGLIO TOT.GEN. = CORRIERE
348600940323     O**COLLI
348700940323     O          E            DETTOT      1
348800970203     O                                           11 'CORR'
348900970203     O                       TKA(1)        Z     18
349000940323     O**PESO
349100970203     O                       TKD(1)        4    124
349200970203     O                       TKE(1)        4    134
349300970203     O                       TKF(1)        4    144
349400940323     O**VOLUME
349500970203     O                       TKI(1)        4    165
349600940323     O**SPEDIZIONI
349700970203     O                       TKL(1)        Z    174
349800931008     O*TRATTINI DI SEPARAZIONE DOPO IL TOTALE GENERALE.
349900931008     O          E            DETTOT      1  1
350000931008     O                       SK(5)               66
350100931008     O                       SK(5)              132
350200970306     O                       SK(18)             198
350300931130      *
350400941012     O          E            FINSTA           63
350500931130     O                       SK1(1)              66
350600941012     O          E            FINSTA           64
350700931130     O                       SK1(2)              66
350800931130     O                       SK1(3)             132
350900931130     O                       SK1(4)             198
351000950111     O                       §QTKPM        2     72
351100931130     O                       §3IARR        Z     99
351200941012     O          E            FINSTA           65
351300941012     O                       SK1(7)              66
351400940126      * AVVISO DI FOGLI VIAGGIO APERTI NEL PERIODO
351500941012     O          E            FVAPER      2
351600941012     O                       SK1(5)             115
351700941012     O                       SK1(6)            +  0
351800931008     O****************************************************************
351900931130      *            2         3         4         5         6         7         8
352000940406**   CMD           0123456789012345678901234567890123456789012345678901234567890
352100950209OVRPRTF FILE(PRTF198) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.part.
352200950209')
352300930928**   SK
352400050802XXXXXXXXXXXXXXXXXXXX                                                 1
352500950111   STATISTICA PARTENZE - PARTITO  IL XX/XX/XXXX                      2
352600950111   STATISTICA PARTENZE - PARTITO DAL XX/XX/XXXX  AL XX/XX/XXXX       3
352700970807                      FNLSC1R                             PAG.XXXX   4
352800930928------------------------------------------------------------------   5
352900970502  PUNTO     COLLI  |--- COLLI NON SPUNTATI (PIST.88,89,98,96) ---|
353000970204OPERATIVO            DiskB  % sul    Altri  % sul   Totale  % sul    7
353100970204                           partito         partito         partito   8
353200970131 |----- S P U N T E   P I S T O L A   87 ------| |----- P   E   S    9
353300970204   DiskB  % sul    Altri  % sul   Totale  % sul   FINO 35 OLTRE 35  10
353400970204         partito         partito         partito                    11
353500970306  O -------|     PESO      VOLUME    SPEDIZIONI                     12
353600970305 Q    TOTALE  TASSABILE            Totale  Tot.Da                   13
353700970305                                   Partito Partire                  14
3538009701311234567890 1234567 1234567 123,45% 1234567 123,45% 1234567 123,45%  15
353900970131 1234567 123,45% 1234567 123,45% 1234567 123,45% 1234567,8 1234567  16
354000970305,8 1234567,8 12345678,9 12345,6789 1234567 1234567 1234567 123,45%  17
354100970306--------------------------------------------------                  18
354200931130**
354300940405N.B.
354400940405PESO TASSABILE:  E' CALCOLATO CON UN RAPPORTO PESO/VOLUME  1 MC =
354500940405XX,XXX Q.LI ARROTONDATO AI XXXXXX KG. SUPERIORI, PER LE SOLE SPEDI
354600940405ZIONI CON PESO FINO A KG. 3500, ESCLUSE BOLLE DI RECUPERO E C/SERV
354700940126***   SONO STATI TROVATI ALCUNI FOGLI VIAGGIO APERTI AVENTI DATA P
354800940126ARTENZA COMPRESA NEL PERIODO   ***
354900940406SPEDIZIONI:  DALLA STATISTICA SONO ESLUSE LE BOLLE DI RECUPERO
