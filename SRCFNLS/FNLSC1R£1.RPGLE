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
002900021023     FFNBLT38L  IF   E           K DISK
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
004200090415     Ffista01L  uF   E           K DISK
004300021016     fTntbe01l  uf   e           k Disk
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
011200950111     D  BLTAAS                 1      4  0
011300950111     D  BLTLNP                 5      7  0
011400950111     D  BLTNRS                 8      9  0
011500950111     D  BLTNSP                10     16  0
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
021200930924     C     KBLT30        KLIST
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
024300930924     C                   KFLD                    BLTAAS
024400931008     C                   KFLD                    BLTLNP
024500930924     C                   KFLD                    BLTNRS
024600930924     C                   KFLD                    BLTNSP
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
093000090415     C                   ENDIF
093100090415     c
093200090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
093300090415     c*  in FISTA
093400090415     c                   if        savris='S'
093500090415     c     ksta          delete    fista01l
093600941012     C                   ENDIF
093700940223      *
093800940509     C     ENDLEG        ENDSR
093900930924     C*****************************************************************
094000930924     C* SUBROUTINE ELALNP: ELABORAZIONE FILIALI (LINEE DI PARTENZA)
094100930924     C*****************************************************************
094200930924     C     ELALNP        BEGSR
094300950111     C*
094400931008     C                   Z-ADD     0             FLDTXT
094500931013     C*ELABORO COLLI
094600021023     C     KBLT30        SETLL     FNBLT38L
094700930924     C*
094800021023     C     KBLT30        READE     FNBLT38L                               31
094900930924     C* CICLO LETTURA COLLI
095000970130    1C     *IN31         DOWEQ     *OFF
095100931013     C* VEDERE I DATI  BOLLA SOLO AL CAMBIO BOLLA
095200970130    2C     DSTXT         IFNE      FLDTXT
095300950111     C                   Z-ADD     DSTXT         FLDTXT
095400950111     C     KBLP01        CHAIN     FNBLP01L                           32
095500931130      *
095600930927     C*SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
095700970130    3C     *IN32         IFEQ      *OFF
095800950111     C                   Z-ADD     1             WA                3 0
095900950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
096000950111     C   32              MOVEL     D3A(WA)       DS3A
096100950111     C  N32              CLEAR                   DS3A
096200931130      *
096300970505     C* REPERIMENTO DATI BOLLA
096400931007     C                   EXSR      DTBOL
096500931007     C* REPERIMENTO DATI BOLLA, PER CALCOLO.
096600930927     C                   EXSR      DATBOL
096700931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN UN RECUPERO
096800970130   X3C                   ELSE
096900930924     C                   MOVEL     'R'           §3ARBL
097000970130    3C                   ENDIF
097100970130    2C                   ENDIF
097200930924     C*
097300931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
097400970130    2C     §3ARBL        IFNE      'R'
097500930924     C*ESECUZIONE SUBROUTINE DEI CALCOLI
097600930924     C                   EXSR      CALC
097700970130    2C                   ENDIF
097800930924     C*
097900021023     C     KBLT30        READE     FNBLT38L                               31
098000970130    1C                   ENDDO
098100070523     c
098200070523     c* Se data elaborazione <= data calcolo affidabilità partenza
098300070523     c*  con data ritiro, calcolo a modo vecchio
098400070523     c                   if        ds1ada<§vponpi
098500930927     C***
098600931013     C*ELABORO SPEDIZIONI
098700970129     C**
098800970129     C** I° CICLO --> DUC = 0 DBR <= DATA ELABORAZIONE
098900970204     C*               ESCLUDENDO QUELLE CON DATA BORDERO' = 0
099000970129     C**
099100970129     C                   MOVE      *ZEROS        KDUC
099200970204     C                   MOVE      *ZEROS        KDBR
099300970204     C     KBLP6A        SETGT     FNBLP06L
099400970129     C     KBLP6         READE     FNBLP06L                               31
099500970130    1C     *IN31         DOWEQ     *OFF
099600970203     C     BLPDBR        ANDLE     DS1ADA
099700931130      * TEST SE E' RECUPERO
099800950111     C                   Z-ADD     1             WA
099900950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
100000950111     C   32              MOVEL     D3A(WA)       DS3A
100100950111     C  N32              CLEAR                   DS3A
100200930927     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
100300970203    2C     §3ARBL        IFNE      'R'
100400970304     C     BLPNFV        ANDNE     99999
100500970304     C     BLPNFV        ANDNE     99998
100600990907     C     BLPCBO        ANDNE     '£ '
100700070525     c     blppdr        andgt     0
100800931007     C                   EXSR      CSPE
100900970203    2C                   ENDIF
101000970203     C     KBLP6         READE     FNBLP06L                               31
101100970130    1C                   ENDDO
101200970129     C**
101300970206     C** II° CICLO --> DUC = DATA ELABORAZIONE
101400970129     C**
101500970218     C                   Z-ADD     DS1ADA        KDUC
101600970206     C     KBLP6         SETLL     FNBLP06L
101700970206     C     KBLP6         READE     FNBLP06L                               31
101800970130    1C     *IN31         DOWEQ     *OFF
101900970129      * TEST SE E' RECUPERO
102000970129     C                   Z-ADD     1             WA
102100970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
102200970129     C   32              MOVEL     D3A(WA)       DS3A
102300970129     C  N32              CLEAR                   DS3A
102400970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
102500970130    2C     §3ARBL        IFNE      'R'
102600970129     C                   EXSR      CSPE
102700970130    2C                   ENDIF
102800970206     C     KBLP6         READE     FNBLP06L                               31
102900970130    1C                   ENDDO
103000970129     C**
103100970129     C** III° CICLO --> DUC > DATA ELABORAZIONE; DBR <= DATA ELABORAZ.
103200970129     C**
103300970129     C                   Z-ADD     DS1ADA        BLPDUC
103400970129     C     KBLP6B        SETGT     FNBLP06L
103500970129     C     COMFIL        READE     FNBLP06L                               31
103600970130    1C     *IN31         DOWEQ     *OFF
103700970204     C*
103800970130    2C     BLPDBR        IFGT      DS1ADA
103900970129     C     KBLP6B        SETGT     FNBLP06L
104000970129     C                   GOTO      LEGBLP
104100970204     C                   ELSE
104200970204     C* ESCLUDO SE DBR = 0
104300970204     C     BLPDBR        IFEQ      *ZEROS
104400970204     C     KBLP6C        SETGT     FNBLP06L
104500970204     C                   GOTO      LEGBLP
104600970204     C                   END
104700970130    2C                   END
104800970129      * TEST SE E' RECUPERO
104900970129     C                   Z-ADD     1             WA
105000970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
105100970129     C   32              MOVEL     D3A(WA)       DS3A
105200970129     C  N32              CLEAR                   DS3A
105300970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
105400970130    2C     §3ARBL        IFNE      'R'
105500070525     C     BLPNFV        ANDNE     99999
105600070525     C     BLPNFV        ANDNE     99998
105700070525     C     BLPCBO        ANDNE     '£ '
105800070525     c     blppdr        andgt     0
105900970129     C                   EXSR      CSPE
106000970130    2C                   ENDIF
106100970129     C     LEGBLP        TAG
106200970203     C     COMFIL        READE     FNBLP06L                               31
106300970130    1C                   ENDDO
106400070523     c
106500070523     c                   else
106600070523     c                   exsr      CALSPEDNEW
106700070523     c                   endif
106800930924     C*
106900930924     C                   ENDSR
107000070523     C*****************************************************************
107100070523     C* CALCOLO LE SPEDIZIONI A MODO NUOVO CONSIDERANDO LA DATA
107200070523     C*   RITIRO MERCE
107300070523     C*****************************************************************
107400070523     C     CALSPEDNEW    BEGSR
107500070523     C***
107600070523     C*ELABORO SPEDIZIONI
107700070523     C**
107800070523     C** I° CICLO --> DUC = 0 Drt <= DATA ELABORAZIONE
107900070523     C**
108000070523     C                   MOVE      *ZEROS        KDUC
108100070523     C                   MOVE      *ZEROS        KDrt
108200070523     C     KBLP5A        SETGT     FNBLP05L
108300070523     C     KBLP5         READE     FNBLP05L                               31
108400070523    1C     *IN31         DOWEQ     *OFF
108500070523     C     BLPDrt        ANDLE     DS1ADA
108600070523      * TEST SE E' RECUPERO
108700070523     C                   Z-ADD     1             WA
108800070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
108900070523     C   32              MOVEL     D3A(WA)       DS3A
109000070523     C  N32              CLEAR                   DS3A
109100070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
109200070523    2C     §3ARBL        IFNE      'R'
109300070523     C     BLPNFV        ANDNE     99999
109400070523     C     BLPNFV        ANDNE     99998
109500070523     C     BLPCBO        ANDNE     '£ '
109600070525     C     BLPpdr        ANDgt     0
109700070523     C                   EXSR      CSPE
109800070523    2C                   ENDIF
109900070523     C     KBLP5         READE     FNBLP05L                               31
110000070523    1C                   ENDDO
110100070523     C**
110200070523     C** II° CICLO --> DUC = DATA ELABORAZIONE
110300070523     C**
110400070523     C                   Z-ADD     DS1ADA        KDUC
110500070523     C     KBLP5         SETLL     FNBLP05L
110600070523     C     KBLP5         READE     FNBLP05L                               31
110700070523    1C     *IN31         DOWEQ     *OFF
110800070523      * TEST SE E' RECUPERO
110900070523     C                   Z-ADD     1             WA
111000070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
111100070523     C   32              MOVEL     D3A(WA)       DS3A
111200070523     C  N32              CLEAR                   DS3A
111300070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
111400070523    2C     §3ARBL        IFNE      'R'
111500070525     C     BLPNFV        ANDNE     99999
111600070525     C     BLPNFV        ANDNE     99998
111700070525     C     BLPCBO        ANDNE     '£ '
111800070523     C                   EXSR      CSPE
111900070523    2C                   ENDIF
112000070523     C     KBLP5         READE     FNBLP05L                               31
112100070523    1C                   ENDDO
112200070523     C**
112300070523     C** III° CICLO --> DUC > DATA ELABORAZIONE; Drt <= DATA ELABORAZ.
112400070523     C**
112500070523     C                   Z-ADD     DS1ADA        BLPDUC
112600070523     C     KBLP5B        SETGT     FNBLP05L
112700070523     C     COMFIL        READE     FNBLP05L                               31
112800070523    1C     *IN31         DOWEQ     *OFF
112900070523     C*
113000070523    2C     BLPDrt        IFGT      DS1ADA
113100070523     C     KBLP5B        SETGT     FNBLP05L
113200070523     C                   GOTO      LEGBLP5
113300070523   x2C                   ELSE
113400070523      * TEST SE E' RECUPERO
113500070523     C                   Z-ADD     1             WA
113600070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
113700070523     C   32              MOVEL     D3A(WA)       DS3A
113800070523     C  N32              CLEAR                   DS3A
113900070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
114000070523    3C     §3ARBL        IFNE      'R'
114100070525     C     BLPNFV        ANDNE     99999
114200070525     C     BLPNFV        ANDNE     99998
114300070525     C     BLPCBO        ANDNE     '£ '
114400070525     C     BLPpdr        ANDgt     0
114500070523     C                   EXSR      CSPE
114600070523    3C                   ENDIF
114700070523    2C                   ENDIF
114800070523     C     LEGBLP5       TAG
114900070523     C     COMFIL        READE     FNBLP05L                               31
115000070523    1C                   ENDDO
115100070523     C                   ENDSR
115200000831     C*****************************************************************
115300000831     C* ELABORO LE ANOMALIE 105 PER LA LINEA INTERESSATA
115400000831     C*****************************************************************
115500000831     C     ELA105        BEGSR
115600070523     C                   Z-ADD     105           WCAA
115700000831     C     KANM          SETLL     FNANM000
115800000831     C     KANM          READE     FNANM000                               32
115900000831     C*
116000000831     C     *IN32         DOWEQ     *OFF
116100000831     C* SE ANOMAIA CHIUSA VUOL DIRE CHE ADESSOHO LA BOLLA
116200000831     C     ANMDCH        IFEQ      0
116300040601     c     anmfle        andeq     simfel
116400000831     C* SE NON E' UGUALE ALLA LINEA DI PARTENZA, VEDO SE CONFERMABILE
116500000831     C*  DA ME
116600000831     C     ANMFLS        IFNE      COMFIL
116700000831     C* SE E' IN T06 NON MEMORIZZO, ALTRIMENTI MEMORIZZO PER SIMFEL
116800000831     C     ANMFLS        LOOKUP    T06                                    32
116900000831     C     *IN32         IFEQ      *OFF
117000000831     C     COMFIL        ANDEQ     SIMFEL
117100000831     C* NON SAPENDO IL TIPO SERVIZIO, SOMMO NELLA DISTRIBUZIONE
117200000831     C                   ADD       1             SKA(6)
117300000831     C                   ENDIF
117400000831     C*
117500000831     C                   ELSE
117600000831     C                   ADD       1             SKA(6)
117700000831     C                   ENDIF
117800000831     C                   ENDIF
117900000831     C*
118000000831     C     KANM          READE     FNANM000                               32
118100000831     C                   ENDDO
118200000831     C*
118300000831     C                   ENDSR
118400930924     C*****************************************************************
118500931007     C* SUBROUTINE DTBOL: DIFFERENZIA TRA FILIALE E C/SERVIZI
118600931013     C* ESEGUITA SOLO PER BOLLE NORMALI.
118700930927     C*****************************************************************
118800931007     C     DTBOL         BEGSR
118900950111     C*
119000000831     C* VERIFICO SE LNP POSTE
119100000831     C     BLPLNP        IFNE      SAVLNP
119200000831     C     BLPLNP        CHAIN     AZORG01L                           32
119300000831     C  N32              MOVEL     ORGDE3        OG143
119400000831     C   32              CLEAR                   OG143
119500020813     ***C                   MOVEL     §OGPT         LNPPT             1
119600020813     c                   if        §OGntw = 'PPT'
119700020813     c                   movel     'S'           LNPPT             1
119800020813     c                   else
119900020813     c                   clear                   LNPPT
120000020813     c                   endif
120100000831     C                   MOVEL     BLPLNP        SAVLNP
120200000831     C                   ENDIF
120300000831     C*
120400970129     C* MEMORIZZO SE E' DISK B O ALTRI
120500970129     C     BLPSOP        IFEQ      '§'
120600970129     C     BLPNRS        ANDGT     *ZEROS
120700970129     C     BLPSOP        OREQ      '%'
120800970129     C     BLPNRS        ANDGT     *ZEROS
120900970129     C                   SETON                                        05
121000970129     C                   ELSE
121100970129     C                   SETOFF                                       05
121200970129     C                   END
121300970129     C*
121400930927     C*TEST SE E' C/SERVIZI O FILIALE
121500931007     C     §3ARBL        IFEQ      'C'
121600930927     C                   SETON                                        04
121700930927     C                   ELSE
121800930927     C                   SETOFF                                       04
121900930927     C                   ENDIF
122000930927     C*
122100930927     C* SE TIPO SPEDIZIONE E' ESPRESSO
122200930927     C     BLPTSP        IFEQ      'E'
122300930927     C* SE (FILIALE) C/SERVIZI
122400930927     C   04              Z-ADD     2             X
122500930927     C* SE FILIALE
122600930927     C  N04              Z-ADD     5             X
122700950111     C                   ELSE
122800000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
122900950111     C     BLPTSP        IFEQ      'D'
123000000619     C     BLPTSP        OREQ      'P'
123100930927     C* SE (FILIALE) C/SERVIZI
123200930927     C   04              Z-ADD     3             X
123300930927     C* SE FILIALE
123400930927     C  N04              Z-ADD     6             X
123500950111     C                   ELSE
123600950111     C*
123700950111     C* SE (FILIALE) C/SERVIZI
123800950111     C   04              Z-ADD     1             X                 1 0
123900950111     C* SE FILIALE
124000950111     C  N04              Z-ADD     4             X
124100950111     C*
124200930927     C                   ENDIF
124300950111     C                   ENDIF
124400931007     C*
124500940411      * Preparo l'indice per c/servizio di filiale (dal 7 al 9), per le schiere
124600940411     C   04X             ADD       6             Y                 1 0
124700931007     C*
124800931007     C                   ENDSR
124900931007     C************************************************************
125000931007     C* SUBROUTINE DATBOL: REPERIMENTO DATI BOLLA PER CALCOLI.
125100931007     C*****************************************************************
125200931007     C     DATBOL        BEGSR
125300021213     ***C*
125400021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
125500021213     ***C     BLPVLF        DIV(H)    BLPNCL        VOLUM            12 4
125600021213     ***C*
125700021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
125800021213     ***C     BLPPKF        DIV(H)    BLPNCL        PESCOL            8 1
125900021220      *
126000021220      * Verifico i dati misurati con la macchina smistacolli
126100021220      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
126200021220      * e calcolo il VOLUME unitario medio per collo da usare se non
126300021220      * tutti i colli sono stati volumati dalla smistacolli
126400021220      *
126500030213      **** Verifico se sono stati volumati tutti i colli
126600030213     c***                clear                   Wsivol
126700021220     c                   clear                   Wvolu
126800050111     c                   if        BLPncr = BLPncl
126900021220     c                   z-add     BLPvlc        Wvolu
127000030213     c***                eval      Wsivol =  'S'
127100021220   x1c                   else
127200021220if  2c                   if        BLPvlc >= BLPvlf
127300021220     c                   z-add     BLPvlc        Wvolu
127400021220   x2c                   else
127500021220     c                   z-add     BLPvlf        Wvolu
127600021220e   2c                   endif
127700021220e   1c                   endif
127800021220      *
127900021220      * Calcolo il volume medio
128000021220     c     Wvolu         div(H)    BLPncl        wVOLUM
128100021220      *
128200021220      * Verifico i dati misurati con la macchina smistacolli
128300021220      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
128400021220      * e calcolo il PESO   unitario medio per collo da usare se non
128500021220      * tutti i colli sono stati pesati   dalla smistacolli
128600021220      *
128700030213      **** Verifico se sono stati pesati   tutti i colli
128800030213     c***                clear                   Wsipes
128900021220     c                   clear                   Wpeso
129000050111     c                   if        BLPncp = BLPncl
129100021220     c                   z-add     BLPpkc        Wpeso
129200030213     c***                eval      Wsipes =  'S'
129300021220   c1c                   else
129400021220if  2c                   if        BLPpkc >= BLPpkf
129500021220     c                   z-add     BLPpkc        Wpeso
129600021220   x2c                   else
129700021220     c                   z-add     BLPpkf        Wpeso
129800021220e   2c                   endif
129900021220e   1c                   endif
130000021220      *
130100021220      * Calcolo il peso medio
130200021220     c     Wpeso         div(H)    BLPncl        wPESCOL
130300940328     C*
130400930927     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
130500021213     ***C     BLPPKF        IFLE      3500
130600021213     C     Wpeso         IFLE      3500
130700930927     C                   SETON                                        06
130800930927     C                   ELSE
130900930927     C                   SETOFF                                       06
131000930927     C                   ENDIF
131100930927     C*
131200931006     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
131300021213     ***C                   Z-ADD     BLPPKF        PESTAX            8 0
131400021213     C                   z-add     Wpeso         PESTAX
131500930927     C*
131600930927     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
131700021213     ***C     BLPVLF        MULT      §QTKPM        COMPKT
131800021213     C     Wvolu         MULT      §QTKPM        COMPKT
131900930927     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
132000930927     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
132100930927     C     COMPKT        IFGT      PESTAX
132200930927     C                   Z-ADD     COMPKT        PESTAX
132300930927     C                   END
132400930930     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
132500930927     C                   Z-ADD     0             COMPKT            8 0
132600930927     C     PESTAX        DIV       §3IARR        COMPKT
132700930927     C                   MVR                     RESTO             3 0
132800930927     C     RESTO         IFGT      0
132900930927     C     COMPKT        MULT      §3IARR        PESTAX
133000930927     C                   ADD       §3IARR        PESTAX
133100930930     C                   ENDIF
133200021213     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
133300021213     C     PESTAX        DIV(H)    BLPNCL        PESTAS
133400930927     C*
133500930927     C                   ENDSR
133600930927     C****************************************************************
133700930929     C* SUBROUTINE CALC:
133800930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
133900930927     C*****************************************************************
134000930927     C     CALC          BEGSR
134100930927     C*
134200930927     C******COLLI
134300930927     C                   ADD       1             SKA(X)
134400941012     C   04              ADD       1             SKA(Y)
134500930927     C*
134600930927     C******COLLI NON SPUNTATI IN PARTENZA: 88.
134700930927     C     BLTNPP        IFEQ      88
134800950328     C     BLTNPP        OREQ      89
134900970430     C     BLTNPP        OREQ      98
135000970430     C     BLTNPP        OREQ      96
135100000831     C* SOLO PER LE BOLLE POSTE, CONTO ANCHE I COLLI CON NPS=90
135200000831     C     LNPPT         OREQ      'S'
135300000831     C     BLTNPP        ANDEQ     90
135400970129     C* DISK B
135500970129     C     *IN05         IFEQ      *ON
135600970129     C*SE (FILIALE) C/SERVIZI
135700970129     C   04              ADD       1             PB88CS            7 0
135800970129     C   04              ADD       1             PB88              7 0
135900970129     C*SE FILIALE
136000970129     C  N04              ADD       1             PB88F             7 0
136100970129     C                   ELSE
136200970129     C* NO DISK B
136300970129     C*
136400930927     C*SE (FILIALE) C/SERVIZI
136500930927     C   04              ADD       1             PI88CS            7 0
136600941012     C   04              ADD       1             PS88              7 0
136700930927     C*SE FILIALE
136800930927     C  N04              ADD       1             PI88              7 0
136900970129     C                   END
137000930927     C                   ENDIF
137100950622     C******COLLI PISTOLA 87 (EX 88)
137200950622     C     BLTNPP        IFEQ      87
137300970129     C* DISK B
137400970129     C     *IN05         IFEQ      *ON
137500970129     C*SE (FILIALE) C/SERVIZI
137600970129     C   04              ADD       1             PB87CS            7 0
137700970129     C   04              ADD       1             PB87              7 0
137800970129     C*SE FILIALE
137900970129     C  N04              ADD       1             PB87F             7 0
138000970129     C                   ELSE
138100970129     C* NO DISK B
138200970129     C*
138300950622     C*SE (FILIALE) C/SERVIZI
138400950622     C   04              ADD       1             PI87CS            7 0
138500950622     C   04              ADD       1             PS87              7 0
138600950622     C*SE FILIALE
138700950622     C  N04              ADD       1             PI87              7 0
138800970129     C                   END
138900950622     C                   ENDIF
139000930927     C*
139100930927     C******PESO
139200930927     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
139300030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
139400030213      * Peso medio
139500030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
139600030213if  1c***                if        Wsipes = 'S'
139700030213     c***                z-add(H)  BLTpuc        PESCOL
139800030213   x1c***                else
139900021220     c                   z-add     Wpescol       PESCOL
140000030213e   1c***                endif
140100930927     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
140200930927     C   06              ADD       PESCOL        SKD(X)
140300941012     C   06
140400941012     CAN 04              ADD       PESCOL        SKD(Y)
140500970129     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
140600970129     C  N06              ADD       PESCOL        SKE(X)
140700970129     C  N06
140800970129     CAN 04              ADD       PESCOL        SKE(Y)
140900930927     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
141000930927     C                   ADD       PESCOL        SKF(X)
141100941012     C   04              ADD       PESCOL        SKF(Y)
141200930927     C*
141300930927     C******PESO TASSABILE
141400931007     C  N04
141500931007     CAN 06              ADD       PESTAS        PETS              9 1
141600931130      *
141700931130      ***** VOLUME
141800021220     *** * Se nel collo c'e' il volume utilizzo quello, altrimenti quello
141900021220     *** *  generico
142000021213     ***C     BLTVUC        IFNE      *ZERO
142100021213     ***C                   Z-ADD(H)  BLTVUC        WVOLUM
142200021213     ***C                   ELSE
142300021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
142400021213     ***C                   ENDIF
142500021220     ***C                   ADD       WVOLUM        SKI(X)
142600021220     ***C   04              ADD       WVOLUM        SKI(Y)
142700021220     *** *
142800021220      * VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
142900030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
143000030213      * Volume medio
143100030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
143200030213if  1c***                if        Wsivol = 'S'
143300030213     c***                z-add     BLTvuc        Volum
143400030213   x1c***                else
143500021220     c                   z-add     Wvolum        Volum
143600030213e   1c***                endif
143700021220     c                   add       VOLUM         SKI(X)
143800021220     c   04              add       VOLUM         SKI(Y)
143900931130      *
144000930927     C                   ENDSR
144100930927     C****************************************************************
144200931007     C* SUBROUTINE CSPE:  CALCOLO SPEDIZIONI
144300930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
144400930927     C*****************************************************************
144500931007     C     CSPE          BEGSR
144600930927     C*
144700931006     C*TEST SE E' C/SERVIZI O FILIALE
144800970228    1C     §3ARBL        IFEQ      'C'
144900931006     C*SE E' C/SERVIZI
145000931006     C                   SETON                                        04
145100970228   x1C                   ELSE
145200931006     C*SE E' FILIALE
145300931006     C                   SETOFF                                       04
145400970228    1C                   ENDIF
145500931006     C*
145600931006     C*FISSO GLI INDICI DI SCHIERA....
145700931006     C*
145800931006     C* SE TIPO SPEDIZIONE E' ESPRESSO
145900970228    1C     BLPTSP        IFEQ      'E'
146000931006     C* SE (FILIALE) C/SERVIZI
146100931006     C   04              Z-ADD     2             X
146200931006     C* SE FILIALE
146300931006     C  N04              Z-ADD     5             X
146400970228   X1C                   ELSE
146500000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
146600970228    2C     BLPTSP        IFEQ      'D'
146700000619    2C     BLPTSP        OREQ      'P'
146800931006     C* SE (FILIALE) C/SERVIZI
146900931006     C   04              Z-ADD     3             X
147000931006     C* SE FILIALE
147100931006     C  N04              Z-ADD     6             X
147200970228   x2C                   ELSE
147300950111     C*
147400950111     C* SE (FILIALE) C/SERVIZI
147500950111     C   04              Z-ADD     1             X                 1 0
147600950111     C* SE FILIALE
147700950111     C  N04              Z-ADD     4             X
147800950111     C*
147900970228    2C                   ENDIF
148000970228    1C                   ENDIF
148100940411      *
148200941012      * Se e' c/servizio aggiungo anche nel totale filiale c/serv.
148300941012     C   04X             ADD       6             Y
148400970228     C*
148500970206     C* Richiamo pgm per calcolo affidabilità
148600970303     C     BLPDCE        IFEQ      *ZEROS
148700970303     C     BLPDUC        ORNE      DS1ADA
148800970206     C                   MOVEL     ' '           D98TLA
148900970206     C                   MOVEL     'P'           D98TBO
149000970206     C                   MOVEL     BLPAAS        D98AAS
149100970206     C                   MOVEL     BLPLNP        D98LNP
149200970206     C                   MOVEL     BLPNRS        D98NRS
149300970206     C                   MOVEL     BLPNSP        D98NSP
149400970310     C                   Z-ADD     DS1ADA        D98DRF
149500970310     C                   Z-ADD     2359          D98ORF
149600970925     C                   CALL      'TNSD99R'
149700970925     C                   PARM                    DS99
149800970303     C                   ELSE
149900970303     C                   Z-ADD     BLPNPI        D98NPI
150000970303     C                   END
150100970206     C*
150200930927     C******SPEDIZIONI
150300970206    1C     BLPDUC        IFEQ      DS1ADA
150400970206     C*  PARTITO
150500930927     C                   ADD       1             SKL(X)
150600941012     C   04              ADD       1             SKL(Y)
150700970206     C*  PARTITO FUORI AFFIDABILITA'
150800970228     C* (SKO LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
150900970206    2C     D98NPI        IFGT      0
151000970206     C                   ADD       1             SKO(X)
151100970206     C   04              ADD       1             SKO(Y)
151200970206     C                   ADD       1             SKN(X)
151300970206     C   04              ADD       1             SKN(Y)
151400970206    2C                   END
151500970206   X1C                   ELSE
151600970206     C* A TERRA FUORI AFFIDABILITA'
151700970228     C* (SKP LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
151800970206    2C     D98NPI        IFGE      0
151900970206     C                   ADD       1             SKP(X)
152000970206     C   04              ADD       1             SKP(Y)
152100970206     C                   ADD       1             SKN(X)
152200970206     C   04              ADD       1             SKN(Y)
152300970206    2C                   END
152400970206    1C                   END
152500970129     C*  DA PARTIRE
152600970206     C                   MOVE      *BLANKS       COMDAP            1
152700970228     C* La distribuzione a terra del giorno non spareggiata non viene
152800970228     C* conteggiata nel "Da partire"
152900970303     C     BLPDUC        IFEQ      *ZEROS
153000970228     C     BLPDPC        ANDEQ     0
153100970206     C     D98NPI        ANDLT     0
153200970228     C*
153300970303     C     BLPDUC        OREQ      *ZEROS
153400970228     C     BLPDPC        ANDGT     DS1ADA
153500970228     C     D98NPI        ANDLT     0
153600970228     C*
153700970303     C     BLPDUC        ORGT      DS1ADA
153800970228     C     BLPDPC        ANDGT     DS1ADA
153900970228     C     D98NPI        ANDLT     0
154000970228     C*
154100970206     C                   MOVE      'N'           COMDAP
154200970206     C                   END
154300970206     C*
154400970206     C     COMDAP        IFEQ      *BLANKS
154500970129     C                   ADD       1             SKM(X)
154600970129     C   04              ADD       1             SKM(Y)
154700970206     C                   END
154800970228     C*
154900930927     C*
155000930927     C                   ENDSR
155100930927     C****************************************************************
155200930927     C* SUBROUTINE TOTLNP:
155300930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
155400930927     C*****************************************************************
155500930927     C     TOTLNP        BEGSR
155600950111     C*
155700930927     C***CALCOLO TOTALI COLLI
155800930927     C*TOTALE COLLI
155900930927     C     SKA(4)        ADD       SKA(5)        SKA456            7 0
156000930927     C                   ADD       SKA(6)        SKA456
156100930927     C**** **** **** **** **** **** **** **** ****
156200930927     C*
156300970129     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
156400930930     C     SKA456        IFNE      0
156500970129     C* DISK B
156600970203     C     PB88F         DIV(H)    SKA456        $XX               7 4
156700970129     C     $XX           MULT      100           PC88B             5 2
156800970129     C* ALTRI
156900970129     C     PI88          DIV(H)    SKA456        $XX
157000970129     C     $XX           MULT      100           PC88              5 2
157100970129     C* TOTALE
157200970129     C     PB88F         ADD       PI88          PI88T             7 0
157300970129     C     PI88T         DIV(H)    SKA456        $XX
157400970129     C     $XX           MULT      100           PC88T             5 2
157500930930     C                   ENDIF
157600931005     C*
157700970129     C                   CLEAR                   PER3T
157800970129     C     PB88F         IFNE      0
157900970129     C                   MOVE      '%'           PER3B             1
158000970129     C                   MOVE      '%'           PER3T             1
158100970129     C                   ELSE
158200970129     C                   MOVE      *BLANKS       PER3B
158300970129     C                   ENDIF
158400970129     C*
158500931005     C     PI88          IFNE      0
158600931005     C                   MOVE      '%'           PER3              1
158700970129     C                   MOVE      '%'           PER3T             1
158800931005     C                   ELSE
158900931005     C                   MOVE      *BLANKS       PER3
159000931005     C                   ENDIF
159100950622     C*
159200950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
159300950622     C     SKA456        IFNE      0
159400970129     C* DISK B
159500970129     C     PB87F         DIV(H)    SKA456        $XX
159600970129     C     $XX           MULT      100           PC87B             5 2
159700970129     C* ALTRI
159800950622     C     PI87          DIV(H)    SKA456        $XX
159900950622     C     $XX           MULT      100           PC87              5 2
160000970129     C* TOTALI
160100970129     C     PB87F         ADD       PI87          PI87T             7 0
160200970129     C     PI87T         DIV(H)    SKA456        $XX
160300970129     C     $XX           MULT      100           PC87T             5 2
160400950622     C                   ENDIF
160500950622     C*
160600970129     C                   CLEAR                   PER34T
160700950622     C     PI87          IFNE      0
160800950622     C                   MOVE      '%'           PER34             1
160900970129     C                   MOVE      '%'           PER34T            1
161000950622     C                   ELSE
161100950622     C                   MOVE      *BLANKS       PER34
161200950622     C                   ENDIF
161300970129     C     PB87F         IFNE      0
161400970129     C                   MOVE      '%'           PER34B            1
161500970129     C                   MOVE      '%'           PER34T            1
161600970129     C                   ELSE
161700970129     C                   MOVE      *BLANKS       PER34B
161800970129     C                   ENDIF
161900930927     C**** **** **** **** **** **** **** **** ****
162000930927     C***CALCOLO TOTALI PESO
162100930927     C*TOTALE PESO FINO A 35 QUINTALI
162200930927     C     SKD(4)        ADD       SKD(5)        SKD456            8 1
162300930927     C                   ADD       SKD(6)        SKD456
162400930927     C*TOTALE PESO OLTRE  35 QUINTALI
162500930927     C     SKE(4)        ADD       SKE(5)        SKE456            8 1
162600930927     C                   ADD       SKE(6)        SKE456
162700930927     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
162800930927     C     SKF(4)        ADD       SKF(5)        SKF456            8 1
162900930927     C                   ADD       SKF(6)        SKF456
163000930927     C**** **** **** **** **** **** **** **** ****
163100970129     C***CALCOLO TOTALE VOLUME
163200930927     C*TOTALE VOLUMI
163300941005     C     SKI(4)        ADD       SKI(5)        SKI456            9 4
163400930927     C                   ADD       SKI(6)        SKI456
163500930927     C**** **** **** **** **** **** **** **** ****
163600930927     C****CALCOLO % E TOTALI SPEDIZIONI
163700970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
163800970226     C* DA PARTIRE E FUORI AFFIDABILITA'
163900970226     C     WFES          IFEQ      'F'
164000970226     C     SKA(4)        ANDEQ     *ZEROS
164100970226     C     SKA(5)        ANDEQ     *ZEROS
164200970226     C     SKA(6)        ANDEQ     *ZEROS
164300970226     C                   CLEAR                   SKM(4)
164400970226     C                   CLEAR                   SKM(5)
164500970226     C                   CLEAR                   SKM(6)
164600970226     C                   CLEAR                   SKN(4)
164700970226     C                   CLEAR                   SKN(5)
164800970226     C                   CLEAR                   SKN(6)
164900970226     C                   CLEAR                   SKP(4)
165000970226     C                   CLEAR                   SKP(5)
165100970226     C                   CLEAR                   SKP(6)
165200970226     C                   END
165300930927     C***CALCOLO TOTALI SPEDIZIONI
165400970129     C*TOTALE SPEDIZIONI PARTITE
165500930927     C     SKL(4)        ADD       SKL(5)        SKL456            7 0
165600930927     C                   ADD       SKL(6)        SKL456
165700970305     C*TOTALE SPEDIZIONI DA PARTIRE
165800970129     C     SKM(4)        ADD       SKM(5)        SKM456            7 0
165900970129     C                   ADD       SKM(6)        SKM456
166000930927     C**** **** **** **** **** **** **** **** ****
166100930928     C****STAMPA SINGOLA FILIALE :DETTAGLIO E TOTALE.
166200970226     C*
166300931011     C     *INOF         IFEQ      *ON
166400941012     C  N36              EXCEPT    TESTA
166500020813     C                   Z-ADD     1             XD
166600931011     C                   ENDIF
166700931011     C*
166800941012     C  N36              EXCEPT    DETFIL
166900950210     C     DS1RIS        IFNE      'R'
167000941012     C                   EXSR      WRIFIL
167100941012     C                   ENDIF
167200930928     C**** **** **** **** **** **** **** **** ****
167300930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE, IN CAMPI AREA
167400000616     C** SOLO SE DEVO STAMPARE
167500000616     C     DS1RIS        IFNE      ' '
167600930929     C                   ADD       SKA456        AKA123
167700930929     C                   ADD       SKA(4)        AKA(1)
167800930929     C                   ADD       SKA(5)        AKA(2)
167900930929     C                   ADD       SKA(6)        AKA(3)
168000930929     C*
168100930929     C                   ADD       PI88          ARPI88            7 0
168200970129     C                   ADD       PB88F         ARPB88            7 0
168300970130     C                   ADD       PI88T         ARP88T            7 0
168400950622     C                   ADD       PI87          ARPI87            7 0
168500970130     C                   ADD       PB87F         ARPB87            7 0
168600970130     C                   ADD       PI87T         ARP87T            7 0
168700930929     C*
168800930929     C                   ADD       SKD456        AKD123
168900930929     C                   ADD       SKD(4)        AKD(1)
169000930929     C                   ADD       SKD(5)        AKD(2)
169100930929     C                   ADD       SKD(6)        AKD(3)
169200930929     C*
169300930929     C                   ADD       SKE456        AKE123
169400930929     C                   ADD       SKE(4)        AKE(1)
169500930929     C                   ADD       SKE(5)        AKE(2)
169600930929     C                   ADD       SKE(6)        AKE(3)
169700930929     C*
169800930929     C                   ADD       SKF456        AKF123
169900930929     C                   ADD       SKF(4)        AKF(1)
170000930929     C                   ADD       SKF(5)        AKF(2)
170100930929     C                   ADD       SKF(6)        AKF(3)
170200930929     C*
170300931007     C                   ADD       PETS          ARPETS            9 1
170400930929     C*
170500930929     C                   ADD       SKI456        AKI123
170600930929     C                   ADD       SKI(4)        AKI(1)
170700930929     C                   ADD       SKI(5)        AKI(2)
170800930929     C                   ADD       SKI(6)        AKI(3)
170900970129     C*
171000930929     C                   ADD       SKL456        AKL123
171100930929     C                   ADD       SKL(4)        AKL(1)
171200930929     C                   ADD       SKL(5)        AKL(2)
171300930929     C                   ADD       SKL(6)        AKL(3)
171400930929     C*
171500930929     C                   ADD       SKM456        AKM123
171600930929     C                   ADD       SKM(4)        AKM(1)
171700930929     C                   ADD       SKM(5)        AKM(2)
171800930929     C                   ADD       SKM(6)        AKM(3)
171900930929     C*
172000930929     C                   ADD       SKN(4)        AKN(1)
172100930929     C                   ADD       SKN(5)        AKN(2)
172200930929     C                   ADD       SKN(6)        AKN(3)
172300000616     C                   ENDIF
172400930929     C*
172500930929     C**** **** **** **** **** **** **** **** ****
172600930929     C****PULIZIA CAMPI FILIALE (DI TOTALE E DI DETTAGLIO)
172700930929     C*CAMPI DI TOTALE
172800930929     C                   Z-ADD     0             SKA456
172900930929     C                   Z-ADD     0             PI88
173000941012     C                   Z-ADD     0             PS88
173100930929     C                   Z-ADD     0             PC88
173200970129     C                   Z-ADD     0             PB88F
173300970129     C                   Z-ADD     0             PB88
173400970129     C                   Z-ADD     0             PC88B
173500970130     C                   Z-ADD     0             PI88T
173600970129     C                   Z-ADD     0             PC88T
173700950622     C                   Z-ADD     0             PI87
173800950622     C                   Z-ADD     0             PS87
173900950622     C                   Z-ADD     0             PC87
174000970129     C                   Z-ADD     0             PB87F
174100970129     C                   Z-ADD     0             PB87
174200970129     C                   Z-ADD     0             PC87B
174300970130     C                   Z-ADD     0             PI87T
174400970129     C                   Z-ADD     0             PC87T
174500930929     C                   Z-ADD     0             SKD456
174600930929     C                   Z-ADD     0             SKE456
174700930929     C                   Z-ADD     0             SKF456
174800930929     C                   Z-ADD     0             PETS
174900930929     C                   Z-ADD     0             SKI456
175000930929     C                   Z-ADD     0             SKL456
175100930929     C                   Z-ADD     0             SKM456
175200930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
175300930929     C*PER LA STAMPA).
175400931005     C                   MOVEA     *ZEROS        SKA(4)
175500930929     C*
175600931005     C                   MOVEA     *ZEROS        SKD(4)
175700930929     C*
175800931005     C                   MOVEA     *ZEROS        SKE(4)
175900930929     C*
176000931005     C                   MOVEA     *ZEROS        SKF(4)
176100930929     C*
176200931005     C                   MOVEA     *ZEROS        SKI(4)
176300930929     C*
176400931005     C                   MOVEA     *ZEROS        SKL(4)
176500930929     C*
176600931005     C                   MOVEA     *ZEROS        SKM(4)
176700930929     C*
176800931005     C                   MOVEA     *ZEROS        SKN(4)
176900970206     C                   MOVEA     *ZEROS        SKO(4)
177000970206     C                   MOVEA     *ZEROS        SKP(4)
177100930929     C*
177200930927     C                   ENDSR
177300930929     C****************************************************************
177400930929     C* SUBROUTINE TOTCS:
177500930929     C**CALCOLO %, CALCOLO TOTALI C/SERVIZI, STAMPA DETTAGLIO C/SERVIZI,
177600930929     C*****************************************************************
177700930929     C     TOTCS         BEGSR
177800950111     C*
177900930929     C***CALCOLO TOTALI COLLI
178000930929     C*TOTALE COLLI
178100930929     C     SKA(1)        ADD       SKA(2)        SKA123            7 0
178200930929     C                   ADD       SKA(3)        SKA123
178300930929     C**** **** **** **** **** **** **** **** ****
178400970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
178500930930     C     SKA123        IFNE      0
178600970130     C* DISK B
178700970130     C     PB88CS        DIV(H)    SKA123        $XX
178800970130     C     $XX           MULT      100           PC88CB            5 2
178900970130     C* ALTRI
179000931005     C     PI88CS        DIV(H)    SKA123        $XX
179100931005     C     $XX           MULT      100           PC88CS            5 2
179200970130     C* TOTALE
179300970130     C     PB88CS        ADD       PI88CS        P88CST            7 0
179400970130     C     P88CST        DIV(H)    SKA123        $XX
179500970130     C     $XX           MULT      100           PC88CT            5 2
179600930930     C                   ENDIF
179700931005     C*
179800970130     C                   CLEAR                   PER3T
179900970130     C     PB88CS        IFNE      0
180000970130     C                   MOVE      '%'           PER3B
180100970130     C                   MOVE      '%'           PER3T
180200931005     C                   ELSE
180300970130     C                   MOVE      *BLANKS       PER3B
180400931005     C                   ENDIF
180500970130     C     PI88CS        IFNE      0
180600970130     C                   MOVE      '%'           PER3
180700970130     C                   MOVE      '%'           PER3T
180800970130     C                   ELSE
180900970130     C                   MOVE      *BLANKS       PER3
181000970130     C                   ENDIF
181100950622     C****CALCOLO % COLLI NON SPUNTATI CON PISTOLA 87 (EX 88)
181200950622     C     SKA123        IFNE      0
181300970130     C* DISK B
181400970130     C     PB87CS        DIV(H)    SKA123        $XX
181500970130     C     $XX           MULT      100           PC87CB            5 2
181600970130     C* ALTRI
181700950622     C     PI87CS        DIV(H)    SKA123        $XX
181800950622     C     $XX           MULT      100           PC87CS            5 2
181900970130     C* TOTALI
182000970130     C     PB87CS        ADD       PI87CS        P87CST            7 0
182100970203     C     P87CST        DIV(H)    SKA123        $XX
182200970130     C     $XX           MULT      100           PC87CT            5 2
182300950622     C                   ENDIF
182400950622     C*
182500970130     C                   CLEAR                   PER34T
182600970130     C     PB87CS        IFNE      0
182700970130     C                   MOVE      '%'           PER34B
182800970130     C                   MOVE      '%'           PER34T
182900950622     C                   ELSE
183000970130     C                   MOVE      *BLANKS       PER34B
183100950622     C                   ENDIF
183200970130     C     PI87CS        IFNE      0
183300970130     C                   MOVE      '%'           PER34
183400970130     C                   MOVE      '%'           PER34T
183500970130     C                   ELSE
183600970130     C                   MOVE      *BLANKS       PER34
183700970130     C                   ENDIF
183800930929     C**** **** **** **** **** **** **** **** ****
183900930929     C***CALCOLO TOTALI PESO
184000930929     C*TOTALE PESO FINO A 35 QUINTALI
184100930929     C     SKD(1)        ADD       SKD(2)        SKD123            8 1
184200930929     C                   ADD       SKD(3)        SKD123
184300930929     C*TOTALE PESO OLTRE  35 QUINTALI
184400930929     C     SKE(1)        ADD       SKE(2)        SKE123            8 1
184500930929     C                   ADD       SKE(3)        SKE123
184600930929     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
184700930929     C     SKF(1)        ADD       SKF(2)        SKF123            8 1
184800930929     C                   ADD       SKF(3)        SKF123
184900930929     C**** **** **** **** **** **** **** **** ****
185000930929     C***CALCOLO TOTALI VOLUMI
185100930929     C*TOTALE VOLUMI
185200941005     C     SKI(1)        ADD       SKI(2)        SKI123            9 4
185300930929     C                   ADD       SKI(3)        SKI123
185400930929     C**** **** **** **** **** **** **** **** ****
185500930929     C****CALCOLO % E TOTALI SPEDIZIONI
185600970130     C***CALCOLO % SPEDIZIONI FUORI AFFIDABILITA'
185700970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
185800970226     C* DA PARTIRE E FUORI AFFIDABILITA'
185900970226     C     WFES          IFEQ      'F'
186000970226     C     SKA(1)        ANDEQ     *ZEROS
186100970226     C     SKA(2)        ANDEQ     *ZEROS
186200970226     C     SKA(3)        ANDEQ     *ZEROS
186300970226     C                   CLEAR                   SKM(1)
186400970226     C                   CLEAR                   SKM(2)
186500970226     C                   CLEAR                   SKM(3)
186600970226     C                   CLEAR                   SKN(1)
186700970226     C                   CLEAR                   SKN(2)
186800970226     C                   CLEAR                   SKN(3)
186900970226     C                   CLEAR                   SKP(1)
187000970226     C                   CLEAR                   SKP(2)
187100970226     C                   CLEAR                   SKP(3)
187200970226     C                   END
187300930929     C*
187400930929     C***CALCOLO TOTALI SPEDIZIONI
187500970130     C*TOTALE SPEDIZIONI PARTITE
187600930929     C     SKL(1)        ADD       SKL(2)        SKL123            7 0
187700930929     C                   ADD       SKL(3)        SKL123
187800970130     C*TOTALE SPEDIZIONI DA PARTIRE
187900930929     C     SKM(1)        ADD       SKM(2)        SKM123            7 0
188000930929     C                   ADD       SKM(3)        SKM123
188100930929     C**** **** **** **** **** **** **** **** ****
188200930929     C****STAMPA FILIALE C/SERVIZI: DETTAGLIO E TOTALE.
188300931011     C     *INOF         IFEQ      *ON
188400941012     C  N36              EXCEPT    TESTA
188500020813     C                   Z-ADD     0             XD
188600931011     C                   ENDIF
188700931011     C*
188800941012     C  N36              EXCEPT    DETCS
188900930929     C**** **** **** **** **** **** **** **** ****
189000930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE C/SERVIZI, IN CAMPI AREA
189100000616     C* SOLO SE STAMPO
189200000616     C     DS1RIS        IFNE      ' '
189300930930     C                   ADD       SKA123        AKA123            7 0
189400930929     C                   ADD       SKA(1)        AKA(1)
189500930929     C                   ADD       SKA(2)        AKA(2)
189600930929     C                   ADD       SKA(3)        AKA(3)
189700930929     C*
189800930929     C                   ADD       PI88CS        ARPI88            7 0
189900970130     C                   ADD       PB88CS        ARPB88            7 0
190000950622     C                   ADD       PI87CS        ARPI87            7 0
190100970130     C                   ADD       PB87CS        ARPB87            7 0
190200970130     C                   ADD       P88CST        ARP88T            7 0
190300970130     C                   ADD       P87CST        ARP87T            7 0
190400930929     C*
190500930930     C                   ADD       SKD123        AKD123            8 1
190600930929     C                   ADD       SKD(1)        AKD(1)
190700930929     C                   ADD       SKD(2)        AKD(2)
190800930929     C                   ADD       SKD(3)        AKD(3)
190900930929     C*
191000930930     C                   ADD       SKE123        AKE123            8 1
191100930929     C                   ADD       SKE(1)        AKE(1)
191200930929     C                   ADD       SKE(2)        AKE(2)
191300930929     C                   ADD       SKE(3)        AKE(3)
191400930929     C*
191500930930     C                   ADD       SKF123        AKF123            8 1
191600930929     C                   ADD       SKF(1)        AKF(1)
191700930929     C                   ADD       SKF(2)        AKF(2)
191800930929     C                   ADD       SKF(3)        AKF(3)
191900930929     C*
192000941005     C                   ADD       SKI123        AKI123            9 4
192100930929     C                   ADD       SKI(1)        AKI(1)
192200930929     C                   ADD       SKI(2)        AKI(2)
192300930929     C                   ADD       SKI(3)        AKI(3)
192400930929     C*
192500930930     C                   ADD       SKL123        AKL123            7 0
192600930930     C                   ADD       SKL(1)        AKL(1)
192700930929     C                   ADD       SKL(2)        AKL(2)
192800930929     C                   ADD       SKL(3)        AKL(3)
192900930929     C*
193000930930     C                   ADD       SKM123        AKM123            7 0
193100930929     C                   ADD       SKM(1)        AKM(1)
193200930929     C                   ADD       SKM(2)        AKM(2)
193300930929     C                   ADD       SKM(3)        AKM(3)
193400930929     C*
193500930929     C                   ADD       SKN(1)        AKN(1)
193600930929     C                   ADD       SKN(2)        AKN(2)
193700930929     C                   ADD       SKN(3)        AKN(3)
193800000616     C                   ENDIF
193900930929     C*
194000930929     C**** **** **** **** **** **** **** **** ****
194100930929     C****PULIZIA CAMPI FILIALE C/SERVIZI (DI TOTALE E DI DETTAGLIO)
194200930929     C*CAMPI DI TOTALE
194300930929     C                   Z-ADD     0             SKA123
194400930929     C                   Z-ADD     0             PI88CS
194500970130     C                   Z-ADD     0             PB88CS
194600930929     C                   Z-ADD     0             PC88CS
194700970130     C                   Z-ADD     0             PC88CB
194800970130     C                   Z-ADD     0             P88CST
194900970130     C                   Z-ADD     0             PC88CT
195000950622     C                   Z-ADD     0             PI87CS
195100970130     C                   Z-ADD     0             PB87CS
195200950622     C                   Z-ADD     0             PC87CS
195300970130     C                   Z-ADD     0             PC87CB
195400970130     C                   Z-ADD     0             P87CST
195500970130     C                   Z-ADD     0             PC87CT
195600930929     C                   Z-ADD     0             SKD123
195700930929     C                   Z-ADD     0             SKE123
195800930929     C                   Z-ADD     0             SKF123
195900930929     C                   Z-ADD     0             SKI123
196000930929     C                   Z-ADD     0             SKL123
196100930929     C                   Z-ADD     0             SKM123
196200930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
196300930929     C*PER LA STAMPA).
196400930929     C*
196500931005     C                   MOVEA     *ZEROS        SKA(1)
196600930929     C*
196700931005     C                   MOVEA     *ZEROS        SKD(1)
196800930929     C*
196900931005     C                   MOVEA     *ZEROS        SKE(1)
197000930929     C*
197100931005     C                   MOVEA     *ZEROS        SKF(1)
197200930929     C*
197300931005     C                   MOVEA     *ZEROS        SKI(1)
197400930929     C*
197500931005     C                   MOVEA     *ZEROS        SKL(1)
197600930929     C*
197700931005     C                   MOVEA     *ZEROS        SKM(1)
197800930929     C*
197900931005     C                   MOVEA     *ZEROS        SKN(1)
198000970206     C                   MOVEA     *ZEROS        SKO(1)
198100970206     C                   MOVEA     *ZEROS        SKP(1)
198200930929     C*
198300930929     C                   ENDSR
198400930929     C****************************************************************
198500930929     C* SUBROUTINE TOTARE:
198600930929     C**CALCOLO %, CALCOLO TOTALI AREA, STAMPA DETTAGLIO AREA,
198700930929     C*****************************************************************
198800930929     C     TOTARE        BEGSR
198900970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
199000930930     C     AKA123        IFNE      0
199100970130     C* DISK B
199200970130     C     ARPB88        DIV(H)    AKA123        $XX
199300970130     C     $XX           MULT      100           APC88B            5 2
199400970130     C* ALTRI
199500970130     C     ARPI88        DIV(H)    AKA123        $XX
199600970130     C     $XX           MULT      100           ARPC88            5 2
199700970130     C* TOTALE
199800970130     C     ARP88T        DIV(H)    AKA123        $XX
199900970130     C     $XX           MULT      100           APC88T            5 2
200000930930     C                   ENDIF
200100931005     C*
200200970130     C                   CLEAR                   PER3T
200300970130     C     ARPB88        IFNE      0
200400970130     C                   MOVE      '%'           PER3B
200500970130     C                   MOVE      '%'           PER3T
200600931005     C                   ELSE
200700970130     C                   MOVE      *BLANKS       PER3B
200800931005     C                   ENDIF
200900970130     C     ARPI88        IFNE      0
201000970130     C                   MOVE      '%'           PER3
201100970130     C                   MOVE      '%'           PER3T
201200970130     C                   ELSE
201300970130     C                   MOVE      *BLANKS       PER3
201400970130     C                   ENDIF
201500950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
201600950622     C     AKA123        IFNE      0
201700970130     C* DISK B
201800970130     C     ARPB87        DIV(H)    AKA123        $XX
201900970130     C     $XX           MULT      100           APC87B            5 2
202000970130     C* ALTRI
202100950622     C     ARPI87        DIV(H)    AKA123        $XX
202200950622     C     $XX           MULT      100           ARPC87            5 2
202300970130     C* TOTALE
202400970130     C     ARP87T        DIV(H)    AKA123        $XX
202500970130     C     $XX           MULT      100           APC87T            5 2
202600950622     C                   ENDIF
202700950622     C*
202800970130     C                   CLEAR                   PER34T
202900970130     C     ARPB87        IFNE      0
203000970130     C                   MOVE      '%'           PER34B
203100970130     C                   MOVE      '%'           PER34T
203200950622     C                   ELSE
203300970130     C                   MOVE      *BLANKS       PER34B
203400950622     C                   ENDIF
203500970130     C     ARPI87        IFNE      0
203600970130     C                   MOVE      '%'           PER34
203700970130     C                   MOVE      '%'           PER34T
203800970130     C                   ELSE
203900970130     C                   MOVE      *BLANKS       PER34
204000970130     C                   ENDIF
204100930929     C**** **** **** **** **** **** **** **** ****
204200930929     C****STAMPA AREA :DETTAGLIO E TOTALE.
204300020813     C     XD            IFLE      7
204400941012     C  N36              EXCEPT    DETARE
204500931011     C                   ELSE
204600941012     C  N36              EXCEPT    TESTA
204700941012     C  N36              EXCEPT    DETARE
204800931011     C                   ENDIF
204900930929     C**** **** **** **** **** **** **** **** ****
205000931007     C****SOMMA CAMPI TOTALI AREA, IN CAMPI TOT. GENERALE
205100000616     C* SOLO SE  STAMPO
205200000616     C     DS1RIS        IFNE      ' '
205300931006     C*
205400931008     C                   ADD       AKA123        TKA123            7 0
205500931006     C                   ADD       AKA(1)        TKA(1)
205600931006     C                   ADD       AKA(2)        TKA(2)
205700931006     C                   ADD       AKA(3)        TKA(3)
205800931006     C*
205900970130     C                   ADD       ARP88T        TGP88T            7 0
206000970130     C                   ADD       ARP87T        TGP87T            7 0
206100931006     C*
206200931008     C                   ADD       AKD123        TKD123            8 1
206300931006     C                   ADD       AKD(1)        TKD(1)
206400931006     C                   ADD       AKD(2)        TKD(2)
206500931006     C                   ADD       AKD(3)        TKD(3)
206600931006     C*
206700931008     C                   ADD       AKE123        TKE123            8 1
206800931006     C                   ADD       AKE(1)        TKE(1)
206900931006     C                   ADD       AKE(2)        TKE(2)
207000931006     C                   ADD       AKE(3)        TKE(3)
207100931006     C*
207200931008     C                   ADD       AKF123        TKF123            8 1
207300931006     C                   ADD       AKF(1)        TKF(1)
207400931006     C                   ADD       AKF(2)        TKF(2)
207500931006     C                   ADD       AKF(3)        TKF(3)
207600931006     C*
207700931007     C                   ADD       ARPETS        TGPETS            9 1
207800931006     C*
207900941005     C                   ADD       AKI123        TKI123            9 4
208000931006     C                   ADD       AKI(1)        TKI(1)
208100931006     C                   ADD       AKI(2)        TKI(2)
208200931006     C                   ADD       AKI(3)        TKI(3)
208300931006     C*
208400931008     C                   ADD       AKL123        TKL123            7 0
208500931006     C                   ADD       AKL(1)        TKL(1)
208600931006     C                   ADD       AKL(2)        TKL(2)
208700931006     C                   ADD       AKL(3)        TKL(3)
208800000616     C                   ENDIF
208900931006     C*
209000930930     C**** **** **** **** **** **** **** **** ****
209100930930     C****PULIZIA CAMPI AREA    (DI TOTALE E DI DETTAGLIO)
209200930929     C*CAMPI DI TOTALE
209300930930     C                   Z-ADD     0             AKA123
209400930930     C                   Z-ADD     0             ARPI88
209500970130     C                   Z-ADD     0             ARPB88
209600970130     C                   Z-ADD     0             APC88B
209700970130     C                   Z-ADD     0             ARPC88
209800970130     C                   Z-ADD     0             ARP88T
209900970130     C                   Z-ADD     0             APC88T
210000950622     C                   Z-ADD     0             ARPI87
210100970130     C                   Z-ADD     0             ARPB87
210200950622     C                   Z-ADD     0             ARPC87
210300970130     C                   Z-ADD     0             APC87B
210400970130     C                   Z-ADD     0             ARP87T
210500970130     C                   Z-ADD     0             APC87T
210600930930     C                   Z-ADD     0             AKD123
210700930930     C                   Z-ADD     0             AKE123
210800930930     C                   Z-ADD     0             AKF123
210900930930     C                   Z-ADD     0             ARPETS
211000930930     C                   Z-ADD     0             AKI123
211100930930     C                   Z-ADD     0             AKL123
211200930930     C                   Z-ADD     0             AKM123
211300930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
211400930929     C*PER LA STAMPA).
211500931005     C                   MOVEA     *ZEROS        AKA(1)
211600930929     C*
211700931005     C                   MOVEA     *ZEROS        AKD(1)
211800930929     C*
211900931005     C                   MOVEA     *ZEROS        AKE(1)
212000930929     C*
212100931005     C                   MOVEA     *ZEROS        AKF(1)
212200930929     C*
212300931005     C                   MOVEA     *ZEROS        AKI(1)
212400930929     C*
212500931005     C                   MOVEA     *ZEROS        AKL(1)
212600930929     C*
212700931005     C                   MOVEA     *ZEROS        AKM(1)
212800930929     C*
212900931005     C                   MOVEA     *ZEROS        AKN(1)
213000930929     C*
213100930929     C                   ENDSR
213200930929     C****************************************************************
213300931006     C* SUBROUTINE ELATRA: ELABORAZIONE TRANSITI
213400931006     C*****************************************************************
213500931006     C     ELATRA        BEGSR
213600950111     C*
213700931008     C                   Z-ADD     0             FLDTXT
213800950111     C*
213900950111     C* ?    C O L L I    T R A N S I T O    ?
214000950329     C*
214100931007     C*POSIZ.PER'DATA DA'(DATA USCITA DAL TRANSITO).
214200020823     C     KDTA          SETLL     FNBTT38L
214300020823     C     KDTA          READE     FNBTT38L                               31
214400931006     C* CICLO LETTURA COLLI
214500970130    1C     *IN31         DOWEQ     *OFF
214600931007     C*
214700931013     C* ENTRA SOLO AL CAMBIO BOLLA.
214800970130    2C     DSTXT1        IFNE      FLDTXT
214900950111     C                   Z-ADD     DSTXT1        FLDTXT
215000020823     C     KBTP11        CHAIN     FNBTP11L                           32
215100931130      *
215200931130      * SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
215300970130    3C     *IN32         IFEQ      *OFF
215400950111     C*
215500950111     C                   Z-ADD     1             WA
215600950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
215700950111     C   32              MOVEL     D3A(WA)       DS3A
215800950111     C  N32              CLEAR                   DS3A
215900931130      *
216000950111     C* REPERIM.DATI BOLLA, (CORR.,ESPR.,RAPID,NON DIFFERENZIANDO I C/SERV.)
216100931007     C* QUINDI SOLO PER BOLLE-TRANSITO.
216200931007     C                   EXSR      DTBOT
216300950111     C* REPERIMENTO DATI BOLLA, PER CALCOLO TRANSITI
216400950111     C                   EXSR      DATBOT
216500931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN RECUPERO PER FARLA SCARTARE
216600970130   X3C                   ELSE
216700931006     C                   MOVEL     'R'           §3ARBL
216800970130    3C                   ENDIF
216900970130    2C                   ENDIF
217000931006     C*
217100931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
217200970130    2C     §3ARBL        IFNE      'R'
217300931013     C* CALCOLI-TRANSITI
217400931007     C                   EXSR      CALCTR
217500970130    2C                   ENDIF
217600931006     C*
217700020823     C     KDTA          READE     FNBTT38L                               31
217800970130    1C                   ENDDO
217900970505     C* AGGIUNGO I COLLI IN DISGUIDO
218000970505     C                   ADD       WDIS88        TRPI88
218100000616     C                   CLEAR                   WDIS88
218200931006     C***
218300950111     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
218400950111     C* ?    S P E D I Z I O N I    T R A N S I T O    ?
218500931007     C*
218600950111     C* POSIZIONAMENTO PER DATA INIZIO PERIODO
218700020823     C     KDTA          SETLL     FNBTP15L
218800020823     C     KDTA          READE     FNBTP15L                               31
218900931013     C*
219000970130    1C     *IN31         DOWEQ     *OFF
219100931007     C*
219200931130      * TEST SE E' RECUPERO
219300950111     C                   Z-ADD     1             WA
219400950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
219500950111     C   32              MOVEL     D3A(WA)       DS3A
219600950111     C  N32              CLEAR                   DS3A
219700931006     C*
219800931006     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
219900970130    2C     §3ARBL        IFNE      'R'
220000931007     C                   EXSR      CSPETR
220100970130    2C                   ENDIF
220200931006     C*
220300950111     C*
220400931006     C*LETTURA DENTRO AL CICLO
220500020823     C     KDTA          READE     FNBTP15L                               31
220600970130    1C                   ENDDO
220700931006     C*
220800931006     C                   ENDSR
220900931007     C*****************************************************************
221000931007     C* SUBROUTINE DTBOT: NON DIFFERENZIA TRA FILIALE E C/SERVIZI
221100931007     C*****************************************************************
221200931007     C     DTBOT         BEGSR
221300950111     C*
221400931007     C*TEST SE E' C/SERVIZI O FILIALE
221500931007     C*MI SERVE SOLO PER SAPERE SE ADDIZIONARE O MENO IL PESO TASSABILE.
221600931007     C     §3ARBL        IFEQ      'C'
221700931007     C                   SETON                                        04
221800931007     C                   ELSE
221900931007     C                   SETOFF                                       04
222000931007     C                   ENDIF
222100931007     C*
222200931007     C*FISSO GLI INDICI DI SCHIERA....
222300931007     C*
222400931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
222500950111     C     BTPTSP        IFEQ      'E'
222600931007     C                   Z-ADD     2             X
222700950111     C                   ELSE
222800931007     C*
222900950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
223000950111     C     BTPTSP        IFEQ      'D'
223100931007     C                   Z-ADD     3             X
223200950111     C                   ELSE
223300950111     C                   Z-ADD     1             X
223400931007     C                   ENDIF
223500950111     C                   ENDIF
223600931007     C*
223700931007     C                   ENDSR
223800950111     C************************************************************
223900950111     C* SUBROUTINE DATBOT: REPERIMENTO DATI BOLLA PER CALCOLI (TRANSITI)
224000950111     C*****************************************************************
224100950111     C     DATBOT        BEGSR
224200021213     ***C*
224300021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
224400021213     ***C     BTPPKB        DIV(H)    BTPNCL        PESCOL            8 1
224500021213     *** *
224600021213     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
224700021213     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
224800021213     *** * BTPVLC=Volume rilevato)
224900050111     ***C     BTPNCR        IFEQ      BTPNCL
225000021213     ***C                   Z-ADD     BTPVLC        BTPVLB
225100021213     ***C                   ELSE
225200021213     ***C     BTPVLC        IFGE      BTPVLB
225300021213     ***C                   Z-ADD     BTPVLC        BTPVLB
225400021213     ***C                   ENDIF
225500021213     ***C                   ENDIF
225600021213     ***C*
225700021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
225800021213     ***C     BTPVLB        DIV(H)    BTPNCL        VOLUM            12 4
225900021220      *
226000021220      * Verifico i dati misurati con la macchina smistacolli
226100021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
226200021220      * e calcolo il VOLUME unitario medio per collo da usare se non
226300021220      * tutti i colli sono stati volumati dalla smistacolli
226400021220      *
226500030213      **** Verifico se sono stati volumati tutti i colli
226600030213     c***                clear                   Wsivol
226700021220     c                   clear                   Wvolu
226800050111     c                   IF        BTPncr = BTPncl
226900021220     c                   z-add     BTPvlc        Wvolu
227000030213     c***                eval      Wsivol =  'S'
227100021220   x1c                   else
227200021220if  2c                   if        BTPvlc >= BTPvlb
227300021220     c                   z-add     BTPvlc        Wvolu
227400021220   x2c                   else
227500021220     c                   z-add     BTPvlb        Wvolu
227600021220e   2c                   endif
227700021220e   1c                   endif
227800021220      *
227900021220      * Calcolo il volume medio
228000021220     c     Wvolu         div(H)    BTPncl        wVOLUM
228100021220      *
228200021220      * Verifico i dati misurati con la macchina smistacolli
228300021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
228400021220      * e calcolo il PESO   unitario medio per collo da usare se non
228500021220      * tutti i colli sono stati pesati   dalla smistacolli
228600021220      *
228700030213      **** Verifico se sono stati pesati   tutti i colli
228800030213     c***                clear                   Wsipes
228900021220     c                   clear                   Wpeso
229000050111     c                   If        BTPncp = BTPncl
229100021220     c                   z-add     BTPpkc        Wpeso
229200030213     c***                eval      Wsipes =  'S'
229300021220   x1c                   else
229400021220if  2c                   if        BTPpkc >= BTPpkb
229500021220     c                   z-add     BTPpkc        Wpeso
229600021220   x2c                   else
229700021220     c                   z-add     BTPpkb        Wpeso
229800021220e   2c                   endif
229900021220e   1c                   endif
230000021220      *
230100021220      * Calcolo il peso medio
230200021220     c     Wpeso         div(H)    BTPncl        wPESCOL
230300950111     C*
230400950111     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
230500021213     ***C     BTPPKB        IFLE      3500
230600021213     C     Wpeso         IFLE      3500
230700950111     C                   SETON                                        06
230800950111     C                   ELSE
230900950111     C                   SETOFF                                       06
231000950111     C                   ENDIF
231100950111     C*
231200950111     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
231300021213     ***C                   Z-ADD     BTPPKB        PESTAX            8 0
231400021213     C                   z-add     Wpeso         PESTAX
231500950111     C*
231600950111     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
231700021213     ***C     BTPVLB        MULT      §QTKPM        COMPKT
231800021213     C     Wvolu         MULT      §QTKPM        COMPKT
231900950111     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
232000950111     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
232100950111     C     COMPKT        IFGT      PESTAX
232200950111     C                   Z-ADD     COMPKT        PESTAX
232300950111     C                   END
232400950111     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
232500950111     C                   Z-ADD     0             COMPKT            8 0
232600950111     C     PESTAX        DIV       §3IARR        COMPKT
232700950111     C                   MVR                     RESTO             3 0
232800950111     C     RESTO         IFGT      0
232900950111     C     COMPKT        MULT      §3IARR        PESTAX
233000950111     C                   ADD       §3IARR        PESTAX
233100950111     C                   ENDIF
233200950111     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
233300021213     C     PESTAX        DIV(H)    BTPNCL        PESTAS
233400950111     C*
233500950111     C                   ENDSR
233600931007     C****************************************************************
233700931007     C* SUBROUTINE CALCTR:
233800931007     C* SOMMA VALORI COLLO, IN TRANSITI.
233900931007     C* COLLI;COLLI NON SPUNT;PESO;PESO TASSABILE;VOLUME.
234000931007     C*****************************************************************
234100931007     C     CALCTR        BEGSR
234200950111     C*
234300931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
234400931007     C******COLLI
234500931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
234600931007     C                   ADD       1             XKA(X)
234700931007     C*
234800931007     C******COLLI NON SPUNTATI IN PARTENZA: 88.
234900950111     C     BTTPUT        IFEQ      88
235000950331     C     BTTPUT        OREQ      89
235100970430     C     BTTPUT        OREQ      98
235200970430     C     BTTPUT        OREQ      96
235300941012     C  N04              ADD       1             TRPI88            7 0
235400931007     C                   ENDIF
235500950622     C******COLLI PISTOLA 87 (EX 88 E 89)
235600950622     C     BTTPUT        IFEQ      87
235700950622     C  N04              ADD       1             TRPI87            7 0
235800950622     C                   ENDIF
235900931007     C*
236000931007     C******PESO
236100931007     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
236200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
236300030213      * Peso medio
236400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
236500030213if  1c***                if        Wsipes = 'S'
236600030213     c***                z-add(H)  BTTpuc        PESCOL
236700030213   x1c***                else
236800021220     c                   z-add     Wpescol       PESCOL
236900030213e   1c***                endif
237000950403     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
237100931007     C   06              ADD       PESCOL        XKD(X)
237200931007     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
237300931007     C  N06              ADD       PESCOL        XKE(X)
237400931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
237500931007     C                   ADD       PESCOL        XKF(X)
237600931007     C*
237700931007     C*
237800931007     C******PESO TASSABILE
237900931007     C  N04
238000931007     CAN 06              ADD       PESTAS        TRPETS            9 1
238100931007     C*
238200931007     C*
238300931007     C******VOLUME
238400021213     ***C     BTTVUC        IFGT      0
238500021213     ***C                   Z-ADD(H)  BTTVUC        WVOLUM
238600021213     ***C                   ELSE
238700021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
238800021213     ***C                   ENDIF
238900021220     ***C*
239000021220     ***C                   ADD       WVOLUM        XKI(X)
239100030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
239200030213      * Volume medio
239300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
239400030213if  1c***                if        Wsivol = 'S'
239500030213     c***                z-add     BTTvuc        Volum
239600030213   x1c***                else
239700021220     c                   z-add     Wvolum        Volum
239800030213e   1c***                endif
239900021220     c                   add       VOLUM         XKI(X)
240000931007     C*
240100931007     C                   ENDSR
240200930929     C****************************************************************
240300931007     C* SUBROUTINE CSPETR:  CALCOLO SPEDIZIONI -TRANSITI
240400931007     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
240500931007     C*****************************************************************
240600931007     C     CSPETR        BEGSR
240700931007     C*
240800931007     C*FISSO GLI INDICI DI SCHIERA....
240900931007     C*
241000931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
241100950111     C     BTPTSP        IFEQ      'E'
241200931007     C                   Z-ADD     2             X
241300950111     C                   ELSE
241400931007     C*
241500950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
241600950111     C     BTPTSP        IFEQ      'D'
241700931007     C                   Z-ADD     3             X
241800950111     C                   ELSE
241900950111     C                   Z-ADD     1             X                 1 0
242000931007     C                   ENDIF
242100950111     C                   ENDIF
242200931007     C*
242300931007     C*
242400931007     C******SPEDIZIONI
242500931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
242600931007     C* SOMMO SOLO IN SCHIERA DEL TOTALE
242700931007     C                   ADD       1             XKL(X)
242800931007     C*
242900931007     C                   ENDSR
243000931007     C****************************************************************
243100931007     C* SUBROUTINE TOTTRA:
243200931007     C**CALCOLO TOTALI TRANSITI STAMPA DETTAGLIO TRANSITI
243300931007     C*****************************************************************
243400931007     C     TOTTRA        BEGSR
243500950111     C*
243600931007     C**** **** **** **** **** **** **** **** ****
243700970218     C* AGGIUNGO I COLLI IN DISGUIDO AL TOTALE COLLI CORRIERE
243800970218     C                   ADD       WDIS          XKA(1)
243900000616     C                   CLEAR                   WDIS
244000931007     C****CALCOLO TOTALE COLLI:
244100931007     C     XKA(1)        ADD       XKA(2)        XKA123            7 0
244200931008     C                   ADD       XKA(3)        XKA123
244300950331     C*
244400931007     C**** **** **** **** **** **** **** **** ****
244500931007     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
244600931007     C     XKA123        IFNE      0
244700931007     C     TRPI88        DIV(H)    XKA123        $XX
244800931007     C     $XX           MULT      100           TRPC88            5 2
244900931007     C                   ENDIF
245000931007     C*
245100931008     C     TRPI88        IFNE      0
245200931007     C                   MOVE      '%'           PER3              1
245300931007     C                   ELSE
245400931007     C                   MOVE      *BLANKS       PER3
245500931007     C                   ENDIF
245600950622     C****CALCOLO % COLLI PISTOLA 87 (EX 88 E 89)
245700950622     C     XKA123        IFNE      0
245800950622     C     TRPI87        DIV(H)    XKA123        $XX
245900950622     C     $XX           MULT      100           TRPC87            5 2
246000950622     C                   ENDIF
246100950622     C*
246200950622     C     TRPI87        IFNE      0
246300950622     C                   MOVE      '%'           PER34             1
246400950622     C                   ELSE
246500950622     C                   MOVE      *BLANKS       PER34
246600950622     C                   ENDIF
246700931007     C**** **** **** **** **** **** **** **** ****
246800931007     C****CALCOLO TOTALI PESO:
246900931007     C*
247000931007     C*TOTALE PESO FINO A 35 QUINTALI
247100931007     C     XKD(1)        ADD       XKD(2)        XKD123            8 1
247200931007     C                   ADD       XKD(3)        XKD123
247300931007     C*TOTALE PESO OLTRE  35 QUINTALI
247400931007     C     XKE(1)        ADD       XKE(2)        XKE123            8 1
247500931007     C                   ADD       XKE(3)        XKE123
247600931007     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
247700931007     C     XKF(1)        ADD       XKF(2)        XKF123            8 1
247800931007     C                   ADD       XKF(3)        XKF123
247900931007     C**** **** **** **** **** **** **** **** ****
248000931007     C****CALCOLO TOTALE VOLUME
248100941005     C     XKI(1)        ADD       XKI(2)        XKI123            9 4
248200931007     C                   ADD       XKI(3)        XKI123
248300931007     C**** **** **** **** **** **** **** **** ****
248400931007     C****CALCOLO TOTALE SPEDIZIONI
248500931007     C     XKL(1)        ADD       XKL(2)        XKL123            7 0
248600931007     C                   ADD       XKL(3)        XKL123
248700931007     C**** **** **** **** **** **** **** **** ****
248800931007     C****STAMPA TRANSITI :DETTAGLIO E TOTALE.
248900941012     C  N36              EXCEPT    DETTRA
249000950210     C     DS1RIS        IFNE      'R'
249100941012     C                   EXSR      WRITRA
249200941012     C                   ENDIF
249300931007     C**** **** **** **** **** **** **** **** ****
249400931007     C****SOMMA CAMPI TOTALI TRANSITI IN CAMPI TOTALI TOT.GEN.
249500000616     C**  SOLO SE NON STAMPO
249600000616     C     DS1RIS        IFNE      ' '
249700931007     C                   ADD       XKA123        TKA123
249800931007     C                   ADD       XKA(1)        TKA(1)
249900931007     C                   ADD       XKA(2)        TKA(2)
250000931007     C                   ADD       XKA(3)        TKA(3)
250100931007     C*
250200970130     C                   ADD       TRPI88        TGP88T            7 0
250300970130     C                   ADD       TRPI87        TGP87T            7 0
250400931007     C*
250500931007     C                   ADD       XKD123        TKD123
250600931007     C                   ADD       XKD(1)        TKD(1)
250700931007     C                   ADD       XKD(2)        TKD(2)
250800931007     C                   ADD       XKD(3)        TKD(3)
250900931007     C*
251000931007     C                   ADD       XKE123        TKE123
251100931007     C                   ADD       XKE(1)        TKE(1)
251200931007     C                   ADD       XKE(2)        TKE(2)
251300931007     C                   ADD       XKE(3)        TKE(3)
251400931007     C*
251500931007     C                   ADD       XKF123        TKF123
251600931007     C                   ADD       XKF(1)        TKF(1)
251700931007     C                   ADD       XKF(2)        TKF(2)
251800931007     C                   ADD       XKF(3)        TKF(3)
251900931007     C*
252000931007     C                   ADD       TRPETS        TGPETS            9 1
252100931007     C*
252200931007     C                   ADD       XKI123        TKI123
252300931007     C                   ADD       XKI(1)        TKI(1)
252400931007     C                   ADD       XKI(2)        TKI(2)
252500931007     C                   ADD       XKI(3)        TKI(3)
252600931007     C*
252700931007     C                   ADD       XKL123        TKL123
252800931007     C                   ADD       XKL(1)        TKL(1)
252900931007     C                   ADD       XKL(2)        TKL(2)
253000931007     C                   ADD       XKL(3)        TKL(3)
253100000616     C                   ENDIF
253200931007     C*
253300931007     C**** **** **** **** **** **** **** **** ****
253400931008     C****PULIZIA CAMPI TRANSITO (DI TOTALE E DI DETTAGLIO)
253500931007     C*CAMPI DI TOTALE
253600931007     C                   Z-ADD     0             XKA123
253700931007     C                   Z-ADD     0             TRPI88
253800931007     C                   Z-ADD     0             TRPC88
253900950622     C                   Z-ADD     0             TRPI87
254000950622     C                   Z-ADD     0             TRPC87
254100931007     C                   Z-ADD     0             XKD123
254200931007     C                   Z-ADD     0             XKE123
254300931007     C                   Z-ADD     0             XKF123
254400931007     C                   Z-ADD     0             TRPETS
254500931007     C                   Z-ADD     0             XKI123
254600931007     C                   Z-ADD     0             XKL123
254700931007     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
254800931007     C*PER LA STAMPA).
254900931007     C                   MOVEA     *ZEROS        XKA(1)
255000931007     C*
255100931007     C                   MOVEA     *ZEROS        XKD(1)
255200931007     C*
255300931007     C                   MOVEA     *ZEROS        XKE(1)
255400931007     C*
255500931007     C                   MOVEA     *ZEROS        XKF(1)
255600931007     C*
255700931007     C                   MOVEA     *ZEROS        XKI(1)
255800931007     C*
255900931007     C                   MOVEA     *ZEROS        XKL(1)
256000931007     C*
256100931007     C                   ENDSR
256200931007     C****************************************************************
256300931008     C* SUBROUTINE TOTGEN:
256400931008     C**CALCOLO TOTALI TOT.GEN. STAMPA DETTAGLIO TOT.GEN.
256500931008     C*****************************************************************
256600931008     C     TOTGEN        BEGSR
256700950111     C*
256800931008     C**** **** **** **** **** **** **** **** ****
256900931008     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
257000931008     C     TKA123        IFNE      0
257100970130     C* TOTALE
257200970131     C     TGP88T        DIV(H)    TKA123        $XX
257300970131     C     $XX           MULT      100           TGC88T            5 2
257400931008     C                   ENDIF
257500931008     C*
257600970131     C     TGP88T        IFNE      0
257700970131     C                   MOVE      '%'           PER3              1
257800970131     C                   ELSE
257900970131     C                   MOVE      *BLANKS       PER3
258000970131     C                   ENDIF
258100950622     C****CALCOLO % COLLI CON PISTOLA 87 (EX 88 E 89)
258200950622     C     TKA123        IFNE      0
258300970130     C* TOTALE
258400970131     C     TGP87T        DIV(H)    TKA123        $XX
258500970131     C     $XX           MULT      100           TGC87T            5 2
258600950622     C                   ENDIF
258700950622     C*
258800970131     C     TGP87T        IFNE      0
258900950622     C                   MOVE      '%'           PER34             1
259000950622     C                   ELSE
259100950622     C                   MOVE      *BLANKS       PER34
259200950622     C                   ENDIF
259300931008     C**** **** **** **** **** **** **** **** ****
259400931008     C****STAMPA TOT.GEN. :DETTAGLIO E TOTALE.
259500941012     C  N36              EXCEPT    DETTOT
259600931008     C**** **** **** **** **** **** **** **** ****
259700931008     C****PULIZIA CAMPI TOT.GEN.(DI TOTALE E DI DETTAGLIO)
259800931008     C*CAMPI DI TOTALE
259900931008     C                   Z-ADD     0             TKA123
260000970131     C                   Z-ADD     0             TGP88T
260100970131     C                   Z-ADD     0             TGP87T
260200970131     C                   Z-ADD     0             TGC88T
260300970131     C                   Z-ADD     0             TGC87T
260400931008     C                   Z-ADD     0             TKD123
260500931008     C                   Z-ADD     0             TKE123
260600931008     C                   Z-ADD     0             TKF123
260700931008     C                   Z-ADD     0             TGPETS
260800931008     C                   Z-ADD     0             TKI123
260900931008     C                   Z-ADD     0             TKL123
261000931008     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
261100931008     C*PER LA STAMPA).
261200931008     C                   MOVEA     *ZEROS        TKA(1)
261300931008     C                   MOVEA     *ZEROS        TKD(1)
261400931008     C                   MOVEA     *ZEROS        TKE(1)
261500931008     C                   MOVEA     *ZEROS        TKF(1)
261600931008     C                   MOVEA     *ZEROS        TKI(1)
261700931008     C                   MOVEA     *ZEROS        TKL(1)
261800940126      *
261900941012      * Verifico se vi sono fogli viaggio aperti
262000941012     C                   EXSR      FOGVIA
262100931130      *
262200931008     C                   ENDSR
262300941012      ****************************************************************
262400941012      * Scrivo in archivio il riepilogo per filiale (totali e c/serv.)
262500941012      ****************************************************************
262600941012     C     WRIFIL        BEGSR
262700941012      *
262800941012      * ---------------------- TOTALI DI FILIALE -------------------------------
262900941012      *
263000941012     C                   Z-ADD     SIMFEL        SPAFLE
263100941012     C                   Z-ADD     COMFIL        SPALNP
263200950210     C                   Z-ADD     DS1ADA        SPADRE
263300941012     C                   MOVE      *BLANK        SPATRE
263400941012      * ---- COLLI ----
263500970130     C                   Z-ADD     SKA(5)        SPACPE
263600970130     C                   Z-ADD     SKA(4)        SPACPC
263700970130     C                   Z-ADD     SKA(6)        SPACPD
263800970129     C                   Z-ADD     PB88F         SPA88B
263900970129     C                   Z-ADD     PI88          SPA88A
264000970129     C                   Z-ADD     PB87F         SPA87B
264100970129     C                   Z-ADD     PI87          SPA87A
264200941012      * ---- PESI ----
264300941012     C                   Z-ADD     SKD(5)        SPAPFE
264400970129     C                   Z-ADD     SKD(4)        SPAPFC
264500970129     C                   Z-ADD     SKD(6)        SPAPFD
264600970129     C                   Z-ADD     SKE(5)        SPAPOE
264700970129     C                   Z-ADD     SKE(4)        SPAPOC
264800970129     C                   Z-ADD     SKE(6)        SPAPOD
264900941012     C                   Z-ADD     PETS          SPAPTA
265000941012      * ---- VOLUMI ----
265100970130     C                   Z-ADD     SKI(5)        SPAVPE
265200970130     C                   Z-ADD     SKI(4)        SPAVPC
265300970130     C                   Z-ADD     SKI(6)        SPAVPD
265400941012      * ---- SPEDIZIONI --
265500970129     C                   Z-ADD     SKL(5)        SPASPE
265600970129     C                   Z-ADD     SKL(4)        SPASPC
265700970129     C                   Z-ADD     SKL(6)        SPASPD
265800970129     C                   Z-ADD     SKM(5)        SPASDE
265900970129     C                   Z-ADD     SKM(4)        SPASDC
266000970129     C                   Z-ADD     SKM(6)        SPASDD
266100970206     C                   Z-ADD     SKO(5)        SPASFE
266200970206     C                   Z-ADD     SKO(4)        SPASFC
266300970206     C                   Z-ADD     SKO(6)        SPASFD
266400970206     C                   Z-ADD     SKP(5)        SPATFE
266500970206     C                   Z-ADD     SKP(4)        SPATFC
266600970206     C                   Z-ADD     SKP(6)        SPATFD
266700941012      *
266800941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
266900941012      * riempo il "flag di trasmissione" (SPAFTR)
267000040921     C***  §5CSPA        IFEQ      'S'
267100040921     C***                MOVE      *BLANK        SPAFTR
267200040921     C***                MOVE      *ZEROS        SPADTR
267300040921     C***                ELSE
267400040921     C***                MOVE      'N'           SPAFTR
267500040921     C***                MOVE      *HIVAL        SPADTR
267600040921     C***                ENDIF
267700040921     c                   move      *blanks       spaftr
267800040921     c                   z-add     dateu         spadtr
267900970204     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI SCRIVO RECORD CON
268000970226     C* TIPO = 'F'
268100970226     C     WFES          IFEQ      'F'
268200970226     C     SPACPE        ANDEQ     *ZEROS
268300970204     C     SPACPC        ANDEQ     *ZEROS
268400970204     C     SPACPD        ANDEQ     *ZEROS
268500970204     C                   MOVE      'F'           SPATRE
268600970204     C                   END
268700970204     C                   WRITE     FNSPA000
268800941012      *
268900941012      * --------------------- CONTO SERVIZI DI FILIALE -------------------------
269000941012      *
269100941012      * Finche sono qua (totali di filiale), scrivo anche i c/serv. di filiale
269200941012     C                   Z-ADD     SIMFEL        SPAFLE
269300941012     C                   Z-ADD     COMFIL        SPALNP
269400950210     C                   Z-ADD     DS1ADA        SPADRE
269500941012     C                   MOVE      'C'           SPATRE
269600941012      * ---- COLLI ----
269700970130     C                   Z-ADD     SKA(8)        SPACPE
269800970130     C                   Z-ADD     SKA(7)        SPACPC
269900970130     C                   Z-ADD     SKA(9)        SPACPD
270000970130     C                   Z-ADD     PB88          SPA88B
270100970130     C                   Z-ADD     PS88          SPA88A
270200970130     C                   Z-ADD     PB87          SPA87B
270300970130     C                   Z-ADD     PS87          SPA87A
270400941012      * ---- PESI ----
270500941012     C                   Z-ADD     SKD(8)        SPAPFE
270600970130     C                   Z-ADD     SKD(7)        SPAPFC
270700970130     C                   Z-ADD     SKD(9)        SPAPFD
270800970130     C                   Z-ADD     SKE(8)        SPAPOE
270900970130     C                   Z-ADD     SKE(7)        SPAPOC
271000970130     C                   Z-ADD     SKE(9)        SPAPOD
271100941012     C                   Z-ADD     *ZERO         SPAPTA
271200941012      * ---- VOLUMI ----
271300970130     C                   Z-ADD     SKI(8)        SPAVPE
271400970130     C                   Z-ADD     SKI(7)        SPAVPC
271500970130     C                   Z-ADD     SKI(9)        SPAVPD
271600941012      * ---- SPEDIZIONI --
271700970130     C                   Z-ADD     SKL(8)        SPASPE
271800970130     C                   Z-ADD     SKL(7)        SPASPC
271900970130     C                   Z-ADD     SKL(9)        SPASPD
272000970130     C                   Z-ADD     SKM(8)        SPASDE
272100970130     C                   Z-ADD     SKM(7)        SPASDC
272200970130     C                   Z-ADD     SKM(9)        SPASDD
272300970206     C                   Z-ADD     SKO(8)        SPASFE
272400970206     C                   Z-ADD     SKO(7)        SPASFC
272500970206     C                   Z-ADD     SKO(9)        SPASFD
272600970206     C                   Z-ADD     SKP(8)        SPATFE
272700970206     C                   Z-ADD     SKP(7)        SPATFC
272800970206     C                   Z-ADD     SKP(9)        SPATFD
272900941012      *
273000941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
273100941012      * riempo il "flag di trasmissione" (SPAFTR)
273200040921     C***  §5CSPA        IFEQ      'S'
273300040921     C***                MOVE      *BLANK        SPAFTR
273400040921     C***                MOVE      *ZEROS        SPADTR
273500040921     C***                ELSE
273600040921     C***                MOVE      'N'           SPAFTR
273700040921     C***                MOVE      *HIVAL        SPADTR
273800040921     C***                ENDIF
273900040921     c                   clear                   spaftr
274000040921     c                   move      dateu         spadtr
274100941012      *
274200970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI NON SCRIVO NIENTE
274300970226     C     WFES          IFEQ      'F'
274400970226     C     SPACPE        ANDEQ     *ZEROS
274500970226     C     SPACPC        ANDEQ     *ZEROS
274600970226     C     SPACPD        ANDEQ     *ZEROS
274700970226     C                   ELSE
274800970226     C                   WRITE     FNSPA000
274900970226     C                   END
275000941012      *
275100941012     C                   ENDSR
275200941012      ****************************************************************
275300941012      * Scrivo in archivio il riepilogo per transiti
275400941012      ****************************************************************
275500941012     C     WRITRA        BEGSR
275600941012      *
275700941012     C                   Z-ADD     SIMFEL        SPAFLE
275800941012     C                   Z-ADD     SIMFEL        SPALNP
275900950210     C                   Z-ADD     DS1ADA        SPADRE
276000941012     C                   MOVE      'T'           SPATRE
276100941012      * ---- COLLI ----
276200970130     C                   Z-ADD     XKA(2)        SPACPE
276300970130     C                   Z-ADD     XKA(1)        SPACPC
276400970130     C                   Z-ADD     XKA(3)        SPACPD
276500970130     C* N.B.:Nei transiti non si distinguono i colli disk B dagli altri
276600970130     C                   MOVE      *ZEROS        SPA88B
276700970130     C                   Z-ADD     TRPI88        SPA88A
276800970130     C                   MOVE      *ZEROS        SPA87B
276900970130     C                   Z-ADD     TRPI87        SPA87A
277000941012      * ---- PESI ----
277100941012     C                   Z-ADD     XKD(2)        SPAPFE
277200970203     C                   Z-ADD     XKD(1)        SPAPFC
277300970203     C                   Z-ADD     XKD(3)        SPAPFD
277400970130     C                   Z-ADD     XKE(2)        SPAPOE
277500970130     C                   Z-ADD     XKE(1)        SPAPOC
277600970130     C                   Z-ADD     XKE(3)        SPAPOD
277700941012     C                   Z-ADD     TRPETS        SPAPTA
277800941012      * ---- VOLUMI ----
277900970130     C                   Z-ADD     XKI(2)        SPAVPE
278000970130     C                   Z-ADD     XKI(1)        SPAVPC
278100970130     C                   Z-ADD     XKI(3)        SPAVPD
278200941012      * ---- SPEDIZIONI --
278300970130     C                   Z-ADD     XKL(2)        SPASPE
278400970130     C                   Z-ADD     XKL(1)        SPASPC
278500970130     C                   Z-ADD     XKL(3)        SPASPD
278600970130     C                   Z-ADD     *ZERO         SPASDE
278700970130     C                   Z-ADD     *ZERO         SPASDC
278800970130     C                   Z-ADD     *ZERO         SPASDD
278900970130     C                   Z-ADD     *ZERO         SPASFE
279000970130     C                   Z-ADD     *ZERO         SPASFC
279100970130     C                   Z-ADD     *ZERO         SPASFD
279200941012      *
279300941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
279400941012      * riempo il "flag di trasmissione" (SPAFTR)
279500040921     C***  §5CSPA        IFEQ      'S'
279600040921     C***                MOVE      *BLANKS       SPAFTR
279700040921     C***                MOVE      *ZEROS        SPADTR
279800040921     C***                ELSE
279900040921     C***                MOVE      'N'           SPAFTR
280000040921     C***                MOVE      *HIVAL        SPADTR
280100040921     C***                ENDIF
280200040921     c                   clear                   spaftr
280300040921     c                   z-add     dateu         spadtr
280400941012      *
280500970130     C     SPACPE        IFGT      *ZERO
280600970130     C     SPACPC        ORGT      *ZERO
280700970130     C     SPACPD        ORGT      *ZERO
280800970130     C                   WRITE     FNSPA000
280900941012     C                   ENDIF
281000941012      *
281100941012     C                   ENDSR
281200941012      ****************************************************************
281300970130      * Routine di RISTAMPA: viene semplicemente letto il file FNSPA00F
281400941012      * e stampato, senza alcuna elaborazione di dati.
281500941012      * E' quindi possibile ristampare piu' giorni accorpandoli insieme.
281600941012      * Le routine di RISTAMPA sono simili a quelle del pgm FNLSC2R.
281700941012      ****************************************************************
281800941012     C     RISTAM        BEGSR
281900050829     c                   setoff                                       3534
282000941012      *
282100941012      * Verifico se richiesta una specifica area
282200950210     C     DS1FEL        COMP      *ZERO                                  34
282300050829     c
282400050829     c* Se sono in filiale imposto = simfel
282500050829     c                   if        ds1fel=0 and simfel>0
282600050829     c                   eval      ds1fel=simfel
282700050829     c                   setoff                                       34
282800050829     c                   endif
282900050829     c
283000050829     c* Se devo elaborare una sola linea di partenza, leggo con
283100050829     c*  la linea
283200050829     c                   if        lin(2)=0 and lin(1)>0
283300050829     c                   eval      klin=lin(1)
283400050829     c                   eval      kdre=ds1ada
283500050829     c                   seton                                        35
283600050829     c                   endif
283700050829     c
283800020813     C                   MOVE      *ZEROS        XD
283900941012      *
284000941012     C                   Z-ADD     *ZERO         $PRIM1            1 0
284100941012     C                   Z-ADD     *ZERO         $FILPR            3 0
284200941012     C                   Z-ADD     *ZERO         $FI2PR            3 0
284300941012      *
284400050829     C  n35DS1FEL        SETLL     FNSPA01L
284500050829     C   35kspa          SETLL     FNSPA01L
284600941012     C                   DO        *HIVAL
284700050829     c* 35 on - lettura con lnp
284800050829     c                   if        *in35
284900050829     C     kspa2         READE(N)  FNSPA01L                               33
285000050829     c                   else
285100970130     C  N34DS1FEL        READE(N)  FNSPA01L                               33
285200970130     C   34              READ(N)   FNSPA01L                               33
285300050829     c                   endif
285400941012      *
285500970130     C  N33SPADRE        IFGE      DS1ADA
285600970130     C     SPADRE        ANDLE     DS1AAL
285700941012      *
285800941012      * La linea deve essere presente in LIN
285900050829     c                   if        not *in35 and *in34
286000941012     C     SPALNP        LOOKUP    LIN                                    32
286100050829     c                   else
286200050829     c                   seton                                        32
286300050829     c                   endif
286400941012      *
286500941012     C     *IN32         IFEQ      *ON
286600941012      *
286700941012     C     SPATRE        IFEQ      *BLANK
286800941012      *
286900941012      * Al primo giro memorizzo il "precedente"
287000941012     C     $PRIM1        IFEQ      *ZERO
287100941012     C                   Z-ADD     SPAFLE        $FI2PR
287200941012     C                   Z-ADD     SPALNP        $FILPR
287300941012     C                   Z-ADD     1             $PRIM1
287400941012     C                   ENDIF
287500941012      *
287600941012      * Se cambia filiale stampo i totali per filiale
287700941012     C     SPALNP        IFNE      $FILPR
287800020813     C                   ADD       1             XD
287900941012     C                   Z-ADD     $FILPR        COMFIL
288000941012     C                   MOVE      *BLANKS       DESLNP
288100941012     C     $FILPR        CHAIN     AZORG01L                           99
288200970203     C     *IN99         IFEQ      *OFF
288300970203     C     ORGDE5        IFNE      *BLANKS
288400970203     C                   MOVEL     ORGDE5        DESLNP
288500970203     C                   ELSE
288600970203     C                   MOVEL     ORGDES        DESLNP
288700970203     C                   END
288800970203     C                   END
288900941012     C                   EXSR      TOTLNP
289000941012     C                   Z-ADD     SPALNP        $FILPR
289100941012     C                   ENDIF
289200941012      *
289300941012      * Se cambia area stampo i totali per c/servizio
289400941012     C     SPAFLE        IFNE      $FI2PR
289500941012     C                   EXSR      TOTCS
289600941012     C                   EXSR      TOTARE
289700000308     C  N08              XFOOT     XKA           TOTXKA            7 0
289800000308     C     *IN08         IFEQ      *ON
289900000308     C     TOTXKA        ORGT      0
290000000308     C                   EXSR      TOTTRA
290100000308     C                   ENDIF
290200941012     C                   Z-ADD     SPAFLE        $FI2PR
290300941012     C                   ENDIF
290400000308      *
290500941012     C                   ENDIF
290600941012      *
290700941012      * Aggiorno i totali
290800941012     C                   EXSR      SERTRA
290900941012      *
291000941012     C                   ENDIF
291100941012     C                   ENDIF
291200941012      *
291300941012     C  N33              ENDDO
291400941012      *
291500941012      * Stampo l'ultimo totale filiale ed il c/servizio
291600941012     C                   Z-ADD     $FILPR        COMFIL
291700941012     C                   MOVE      *BLANKS       DESLNP
291800941012     C     $FILPR        CHAIN     AZORG01L                           99
291900970203     C     *IN99         IFEQ      *OFF
292000970203     C     ORGDE5        IFNE      *BLANKS
292100970203     C                   MOVEL     ORGDE5        DESLNP
292200970203     C                   ELSE
292300970203     C                   MOVEL     ORGDES        DESLNP
292400970203     C                   END
292500970203     C                   END
292600941012     C                   EXSR      TOTLNP
292700941012     C                   EXSR      TOTCS
292800941012     C                   EXSR      TOTARE
292900941012      *
293000941012      * Stampo il transito
293100000308     C  N08              XFOOT     XKA           TOTXKA            7 0
293200000308     C     *IN08         IFEQ      *ON
293300000308     C     TOTXKA        ORGT      0
293400000308     C                   EXSR      TOTTRA
293500000308     C                   ENDIF
293600941012      *
293700941012     C     ENDRIS        ENDSR
293800941012      *****************************************************************
293900941012      * RISTAMPA: mantengo aggiornati i totali
294000941012      *****************************************************************
294100941012     C     SERTRA        BEGSR
294200941012      *
294300941012     C     SPATRE        IFEQ      *BLANK
294400941012      * Colli
294500970130     C                   ADD       SPACPC        SKA(4)
294600970130     C                   ADD       SPACPE        SKA(5)
294700970130     C                   ADD       SPACPD        SKA(6)
294800970130     C                   ADD       SPA88B        PB88F
294900970130     C                   ADD       SPA88A        PI88
295000970130     C                   ADD       SPA87B        PB87F
295100970130     C                   ADD       SPA87A        PI87
295200941012      * Pesi
295300970130     C                   ADD       SPAPFC        SKD(4)
295400970130     C                   ADD       SPAPFE        SKD(5)
295500970130     C                   ADD       SPAPFD        SKD(6)
295600970130     C                   ADD       SPAPOC        SKE(4)
295700970130     C                   ADD       SPAPOE        SKE(5)
295800970130     C                   ADD       SPAPOD        SKE(6)
295900970130     C     SKD(4)        ADD       SKE(4)        SKF(4)
296000970130     C     SKD(5)        ADD       SKE(5)        SKF(5)
296100970130     C     SKD(6)        ADD       SKE(6)        SKF(6)
296200941012     C                   ADD       SPAPTA        PETS
296300941012      * Volumi
296400970130     C                   ADD       SPAVPC        SKI(4)
296500970130     C                   ADD       SPAVPE        SKI(5)
296600970130     C                   ADD       SPAVPD        SKI(6)
296700941012      * Spedizioni
296800970130     C                   ADD       SPASPC        SKL(4)
296900970130     C                   ADD       SPASPE        SKL(5)
297000970130     C                   ADD       SPASPD        SKL(6)
297100970130     C                   ADD       SPASDC        SKM(4)
297200970130     C                   ADD       SPASDE        SKM(5)
297300970130     C                   ADD       SPASDD        SKM(6)
297400970206     C     SPASFC        ADD       SPATFC        SKN(4)
297500970206     C     SPASFE        ADD       SPATFE        SKN(5)
297600970206     C     SPASFD        ADD       SPATFD        SKN(6)
297700941012     C                   ENDIF
297800941012      *
297900941012     C     SPATRE        IFEQ      'C'
298000941012      * Colli
298100970130     C                   ADD       SPACPC        SKA(1)
298200970130     C                   ADD       SPACPE        SKA(2)
298300970130     C                   ADD       SPACPD        SKA(3)
298400970130     C                   ADD       SPA88B        PB88CS
298500970130     C                   ADD       SPA88B        PB88
298600970130     C                   ADD       SPA88A        PI88CS
298700970130     C                   ADD       SPA88A        PS88
298800970130     C                   ADD       SPA87B        PB87CS
298900970130     C                   ADD       SPA87B        PB87
299000970130     C                   ADD       SPA87A        PI87CS
299100970130     C                   ADD       SPA87A        PS87
299200941012      * Pesi
299300970130     C                   ADD       SPAPFC        SKD(1)
299400970130     C                   ADD       SPAPFE        SKD(2)
299500970130     C                   ADD       SPAPFD        SKD(3)
299600970130     C                   ADD       SPAPOC        SKE(1)
299700970130     C                   ADD       SPAPOE        SKE(2)
299800970130     C                   ADD       SPAPOD        SKE(3)
299900970130     C     SKD(1)        ADD       SKE(1)        SKF(1)
300000970130     C     SKD(2)        ADD       SKE(2)        SKF(2)
300100970130     C     SKD(3)        ADD       SKE(3)        SKF(3)
300200941012      * Volumi
300300970130     C                   ADD       SPAVPC        SKI(1)
300400970130     C                   ADD       SPAVPE        SKI(2)
300500970130     C                   ADD       SPAVPD        SKI(3)
300600941012      * Spedizioni
300700970130     C                   ADD       SPASPC        SKL(1)
300800970130     C                   ADD       SPASPE        SKL(2)
300900970130     C                   ADD       SPASPD        SKL(3)
301000970130     C                   ADD       SPASDC        SKM(1)
301100970130     C                   ADD       SPASDE        SKM(2)
301200970130     C                   ADD       SPASDD        SKM(3)
301300970228     C     SPATFC        ADD       SPASFC        W0010
301400970228     C     SPATFE        ADD       SPASFE        W0020
301500970228     C     SPATFD        ADD       SPASFD        W0030
301600970228     C                   ADD       W0010         SKN(1)
301700970228     C                   ADD       W0020         SKN(2)
301800970228     C                   ADD       W0030         SKN(3)
301900941012     C                   ENDIF
302000941012      *
302100941012     C     SPATRE        IFEQ      'T'
302200970130     C                   ADD       SPACPC        XKA(1)
302300970130     C                   ADD       SPACPE        XKA(2)
302400970130     C                   ADD       SPACPD        XKA(3)
302500970130     C                   ADD       SPA88A        TRPI88
302600970130     C                   ADD       SPA87A        TRPI87
302700941012      * Pesi
302800970130     C                   ADD       SPAPFC        XKD(1)
302900970130     C                   ADD       SPAPFE        XKD(2)
303000970130     C                   ADD       SPAPFD        XKD(3)
303100970130     C                   ADD       SPAPOC        XKE(1)
303200970130     C                   ADD       SPAPOE        XKE(2)
303300970130     C                   ADD       SPAPOD        XKE(3)
303400970228     C     XKD(1)        ADD       XKE(1)        XKF(1)
303500970228     C     XKD(2)        ADD       XKE(2)        XKF(2)
303600970228     C     XKD(3)        ADD       XKE(3)        XKF(3)
303700941012     C                   ADD       SPAPTA        TRPETS
303800941012      * Volumi
303900970130     C                   ADD       SPAVPC        XKI(1)
304000970130     C                   ADD       SPAVPE        XKI(2)
304100970130     C                   ADD       SPAVPD        XKI(3)
304200941012      * Spedizioni
304300970130     C                   ADD       SPASPC        XKL(1)
304400970130     C                   ADD       SPASPE        XKL(2)
304500970130     C                   ADD       SPASPD        XKL(3)
304600941012     C                   ENDIF
304700941012      *
304800941012     C                   ENDSR
304900941012      ****************************************************************
305000941012      * Verifica se vi sono dei fogli viaggio con data partenza compresa
305100941012      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
305200941012      ****************************************************************
305300941012     C     FOGVIA        BEGSR
305400941012      *
305500950404     C  N36DS1RIS        IFEQ      'S'
305600941012      *
305700020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
305800020813     ***C     *LOVAL        SETLL     FNFGV04L
305900020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
306000020813     C     SIMfel        setll     FNFGV06L
306100941012     C                   DO        *HIVAL
306200020813     ***C                   READ      FNFGV04L                               31
306300020813     C     SIMfel        reade     FNFGV06L                               31
306400950210     C  N31FGVDTP        IFGE      DS1ADA
306500950210     C     FGVDTP        ANDLE     DS1AAL
306600941012     C                   SETON                                        31
306700941012     C                   EXCEPT    FVAPER
306800941012     C                   ENDIF
306900941012     C  N31              ENDDO
307000941012      *
307100941012     C                   ENDIF
307200941012      *
307300941012     C                   ENDSR
307400940103      ****************************************************************
307500941012      * ----------------?    TESTATA  DI  STAMPA    ?-----------------
307600930928     O*PRIMA RIGA SE C'E' SOLO 'DATA DA'
307700930928     OPRTF198   E   N03      TESTA            02
307800930928     O                       SK(1)               66
307900930928     O                       SK(2)              132
308000930928     O                       SK(4)              198
308100930928     O                       RSUT                20
308200941012     O                       STRIP               48
308300950210     O                       DS1GDA             113 '  /  /    '
308400950210     O                       WDATE              181 '  /  /    '
308500930928     O                       PAGE          Z    198
308600930928     O*PRIMA RIGA SE C'E' 'DATA DA' E 'DATA A'
308700930928     O          E    03      TESTA            02
308800930928     O                       SK(1)               66
308900930928     O                       SK(3)              132
309000930928     O                       SK(4)              198
309100930928     O                       RSUT                20
309200930928     O                       SIMFEL              24
309300941012     O                       STRIP               48
309400950210     O                       DS1GDA             113 '  /  /    '
309500950210     O                       DS1GAL             128 '  /  /    '
309600950210     O                       WDATE              181 '  /  /    '
309700930928     O                       PAGE          Z    198
309800930928     O*RIGA IN CUI C'E' SOLO UTIME
309900930928     O          E            TESTA            03
310000950111     O                       KNSIF               10
310100950111     O                       KNMUS               21
310200941012     O                       STRIP1              48
310300950210     O                       UTIME              180 '  :  :  '
310400930928     O*TRATTINI
310500930928     O          E            TESTA            04
310600930928     O                       SK(5)               66
310700930928     O                       SK(5)              132
310800970306     O                       SK(18)             198
310900930928     O*PRIMA RIGA DI COSTANTI
311000930928     O          E            TESTA            05
311100930928     O                       SK(6)               66
311200970131     O                       SK(9)              132
311300970131     O                       SK(12)             198
311400930928     O*SECONDA RIGA DI COSTANTI
311500930928     O          E            TESTA            06
311600970131     O                       SK(7)               66
311700930928     O                       SK(10)             132
311800970131     O                       SK(13)             198
311900930928     O*TERZA RIGA DI COSTANTI
312000930928     O          E            TESTA            07
312100970131     O                       SK(8)               66
312200970131     O                       SK(11)             132
312300930928     O                       SK(14)             198
312400930928     O*TRATTINI
312500970131     O          E            TESTA            08
312600930928     O                       SK(5)               66
312700930928     O                       SK(5)              132
312800970306     O                       SK(18)             198
312900941012      * ----------------?    DETTAGLIO DI FILIALE    ?----------------
313000930928     O****1°RIGA DEL DETTAGLIO FILIALE = TOTALI
313100930928     O**COLLI
313200930929     O          E            DETFIL      2
313300970131     O                       DESLNP              10
313400970131     O                       SKA456        Z     18
313500970131     O*COLLI NO SPUNT. IN PART. DISK B
313600970131     O                       PB88F         Z     26
313700970131     O                       PC88B         4     33
313800970131     O                       PER3B               34
313900970131     O*COLLI NO SPUNT. IN PART. ALTRI
314000970131     O                       PI88          Z     42
314100970131     O                       PC88          4     49
314200970131     O                       PER3                50
314300970131     O*COLLI NO SPUNT. IN PART. TOTALE
314400970131     O                       PI88T         Z     58
314500970131     O                       PC88T         4     65
314600970131     O                       PER3T               66
314700970131     O*COLLI PISTOLA 87 (EX 88) DISK B
314800970131     O                       PB87F         Z     74
314900970131     O                       PC87B         4     81
315000970131     O                       PER34B              82
315100970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
315200970131     O                       PI87          Z     90
315300970131     O                       PC87          4     97
315400970131     O                       PER34               98
315500970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
315600970131     O                       PI87T         Z    106
315700970131     O                       PC87T         4    113
315800970131     O                       PER34T             114
315900930928     O**PESO
316000970131     O                       SKD456        4    124
316100970131     O                       SKE456        4    134
316200970131     O                       SKF456        4    144
316300930928     O**PESO TASSABILE
316400970131     O                       PETS          4    155
316500930928     O**VOLUME
316600970131     O                       SKI456        4    165
316700930928     O**SPEDIZIONI
316800970131     O                       SKL456        Z    174
316900970131     O                       SKM456        Z    182
317000930928     O*
317100931011     O****2°RIGA DEL DETTAGLIO FILIALE = ESPRESSO
317200930928     O**COLLI
317300930929     O          E            DETFIL      1
317400970131     O                                           11 'ESPR'
317500970131     O                       SKA(5)        Z     18
317600930928     O**PESO
317700970131     O                       SKD(5)        4    124
317800970131     O                       SKE(5)        4    134
317900970131     O                       SKF(5)        4    144
318000930928     O**VOLUME
318100970131     O                       SKI(5)        4    165
318200930928     O**SPEDIZIONI
318300970131     O                       SKL(5)        Z    174
318400970131     O                       SKM(5)        Z    182
318500931011     O*
318600950111     O****3°RIGA DEL DETTAGLIO FILIALE = DISTRIBUZIONE
318700930928     O**COLLI
318800930929     O          E            DETFIL      1
318900970131     O                                           11 'DIST'
319000970131     O                       SKA(6)        Z     18
319100930928     O**PESO
319200970131     O                       SKD(6)        4    124
319300970131     O                       SKE(6)        4    134
319400970131     O                       SKF(6)        4    144
319500930928     O**VOLUME
319600970131     O                       SKI(6)        4    165
319700930928     O**SPEDIZIONI
319800970131     O                       SKL(6)        Z    174
319900970131     O                       SKM(6)        Z    182
320000940323     O*
320100950111     O****4°RIGA DEL DETTAGLIO FILIALE = CORRIERE
320200940323     O**COLLI
320300940323     O          E            DETFIL      1
320400970131     O                                           11 'CORR'
320500970131     O                       SKA(4)        Z     18
320600940323     O**PESO
320700970131     O                       SKD(4)        4    124
320800970131     O                       SKE(4)        4    134
320900970131     O                       SKF(4)        4    144
321000940323     O**VOLUME
321100970131     O                       SKI(4)        4    165
321200940323     O**SPEDIZIONI
321300970131     O                       SKL(4)        Z    174
321400970131     O                       SKM(4)        Z    182
321500941012      * ----------?    RIEPILOGO C/SERVIZI PER FILIALE    ?-----------
321600930929     O****1°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = TOTALI
321700930929     O**COLLI
321800970131     O          E            DETCS       2
321900970131     O                                           10 'C/SERVIZI '
322000970131     O                       SKA123        Z     18
322100970131     O*COLLI NO SPUNT. IN PART. DISK B
322200970131     O                       PB88CS        Z     26
322300970131     O                       PC88CB        4     33
322400970131     O                       PER3B               34
322500970131     O*COLLI NO SPUNT. IN PART. ALTRI
322600970131     O                       PI88CS        Z     42
322700970131     O                       PC88CS        4     49
322800970131     O                       PER3                50
322900970131     O*COLLI NO SPUNT. IN PART. TOTALE
323000970131     O                       P88CST        Z     58
323100970131     O                       PC88CT        4     65
323200970131     O                       PER3T               66
323300970131     O*COLLI PISTOLA 87 (EX 88) DISKB
323400970131     O                       PB87CS        Z     74
323500970131     O                       PC87CB        4     81
323600970131     O                       PER34B              82
323700970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
323800970131     O                       PI87CS        Z     90
323900970131     O                       PC87CS        4     97
324000970131     O                       PER34               98
324100970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
324200970131     O                       P87CST        Z    106
324300970131     O                       PC87CT        4    113
324400970131     O                       PER34T             114
324500930929     O**PESO
324600970131     O                       SKD123        4    124
324700970131     O                       SKE123        4    134
324800970131     O                       SKF123        4    144
324900930929     O**PESO TASSABILE (PER C/SERVIZI NON C'E').
325000930929     O**VOLUME
325100970131     O                       SKI123        4    165
325200930929     O**SPEDIZIONI
325300970131     O                       SKL123        Z    174
325400970131     O                       SKM123        Z    182
325500930929     O*
325600931011     O****2°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = ESPRESSO
325700930929     O**COLLI
325800930929     O          E            DETCS       1
325900970131     O                                           11 'ESPR'
326000970131     O                       SKA(2)        Z     18
326100930929     O**PESO
326200970131     O                       SKD(2)        4    124
326300970131     O                       SKE(2)        4    134
326400970131     O                       SKF(2)        4    144
326500930929     O**VOLUME
326600970131     O                       SKI(2)        4    165
326700930929     O**SPEDIZIONI
326800970131     O                       SKL(2)        Z    174
326900970131     O                       SKM(2)        Z    182
327000931011     O*
327100950111     O****3°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = DISTRIBUZIONE
327200930929     O**COLLI
327300930929     O          E            DETCS       1
327400970131     O                                           11 'DIST'
327500970131     O                       SKA(3)        Z     18
327600930929     O**PESO
327700970131     O                       SKD(3)        4    124
327800970131     O                       SKE(3)        4    134
327900970131     O                       SKF(3)        4    144
328000930929     O**VOLUME
328100970131     O                       SKI(3)        4    165
328200930929     O**SPEDIZIONI
328300970131     O                       SKL(3)        Z    174
328400970131     O                       SKM(3)        Z    182
328500940323     O*
328600950111     O****4°RIGA DEL DETTAGLIO FILIALE C/SERVIZI= CORRIERE
328700940323     O**COLLI
328800940323     O          E            DETCS       1
328900970131     O                                           11 'CORR'
329000970131     O                       SKA(1)        Z     18
329100940323     O**PESO
329200970131     O                       SKD(1)        4    124
329300970131     O                       SKE(1)        4    134
329400970131     O                       SKF(1)        4    144
329500940323     O**VOLUME
329600970131     O                       SKI(1)        4    165
329700940323     O**SPEDIZIONI
329800970131     O                       SKL(1)        Z    174
329900970131     O                       SKM(1)        Z    182
330000941012      * ----------------?    RIEPILOGO PER AREA    ?------------------
330100941012     O*TRATTINI DI SEPARAZIONE TRA I  C/SERVIZI E L'AREA
330200941012     O          E            DETARE      1
330300941012     O                       SK(5)               66
330400941012     O                       SK(5)              132
330500970306     O                       SK(18)             198
330600930930     O****1°RIGA DEL DETTAGLIO AREA = TOTALI
330700930930     O**COLLI
330800931011     O          E            DETARE      1
330900970131     O                                           10 'TOTALE    '
331000970131     O                       AKA123        Z     18
331100970131     O*COLLI NO SPUNT. IN PART. DISK B
331200970131     O                       ARPB88        Z     26
331300970131     O                       APC88B        4     33
331400970131     O                       PER3B               34
331500970131     O*COLLI NO SPUNT. IN PART. ALTRI
331600970131     O                       ARPI88        Z     42
331700970131     O                       ARPC88        4     49
331800970131     O                       PER3                50
331900970131     O*COLLI NO SPUNT. IN PART. TOTALE
332000970131     O                       ARP88T        Z     58
332100970131     O                       APC88T        4     65
332200970131     O                       PER3T               66
332300970131     O*COLLI PISTOLA 87 (EX 88) DISK B
332400970131     O                       ARPB87        Z     74
332500970131     O                       APC87B        4     81
332600970131     O                       PER34B              82
332700970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
332800970131     O                       ARPI87        Z     90
332900970131     O                       ARPC87        4     97
333000970131     O                       PER34               98
333100970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
333200970131     O                       ARP87T        Z    106
333300970131     O                       APC87T        4    113
333400970131     O                       PER34T             114
333500930930     O**PESO
333600970131     O                       AKD123        4    124
333700970131     O                       AKE123        4    134
333800970131     O                       AKF123        4    144
333900930930     O**PESO TASSABILE
334000970131     O                       ARPETS        4    155
334100930930     O**VOLUME
334200970131     O                       AKI123        4    165
334300930930     O**SPEDIZIONI
334400970131     O                       AKL123        Z    174
334500970131     O                       AKM123        Z    182
334600930930     O*
334700931011     O****2°RIGA DEL DETTAGLIO AREA = ESPRESSO
334800930930     O**COLLI
334900930930     O          E            DETARE      1
335000970131     O                                           11 'ESPR'
335100970131     O                       AKA(2)        Z     18
335200930930     O**PESO
335300970131     O                       AKD(2)        4    124
335400970131     O                       AKE(2)        4    134
335500970131     O                       AKF(2)        4    144
335600930930     O**VOLUME
335700970131     O                       AKI(2)        4    165
335800930930     O**SPEDIZIONI
335900970131     O                       AKL(2)        Z    174
336000970131     O                       AKM(2)        Z    182
336100931011     O*
336200950111     O****3°RIGA DEL DETTAGLIO AREA = DISTRIBUZIONE
336300930930     O**COLLI
336400940324     O          E            DETARE      1
336500970131     O                                           11 'DIST'
336600970131     O                       AKA(3)        Z     18
336700930930     O**PESO
336800970131     O                       AKD(3)        4    124
336900970131     O                       AKE(3)        4    134
337000970131     O                       AKF(3)        4    144
337100930930     O**VOLUME
337200970131     O                       AKI(3)        4    165
337300930930     O**SPEDIZIONI
337400970131     O                       AKL(3)        Z    174
337500970131     O                       AKM(3)        Z    182
337600940323     O*
337700950111     O****4°RIGA DEL DETTAGLIO AREA = CORRIERE
337800940323     O**COLLI
337900940324     O          E            DETARE      1  1
338000970203     O                                           11 'CORR'
338100970203     O                       AKA(1)        Z     18
338200940323     O**PESO
338300970203     O                       AKD(1)        4    124
338400970203     O                       AKE(1)        4    134
338500970203     O                       AKF(1)        4    144
338600940323     O**VOLUME
338700970203     O                       AKI(1)        4    165
338800940323     O**SPEDIZIONI
338900970203     O                       AKL(1)        Z    174
339000970203     O                       AKM(1)        Z    182
339100941012      * ----------------?    RIEPILOGO TRANSITI    ?------------------
339200931008     O****1°RIGA DEL DETTAGLIO TRANSITI = TOTALI
339300931008     O**COLLI
339400970203     O          E            DETTRA      1
339500970203     O                                           10 'TRANSITI  '
339600970203     O                       XKA123        Z     18
339700931008     O*COLLI NO SPUNT. IN PART.
339800970203     O                       TRPI88        Z     58
339900970203     O                       TRPC88        4     65
340000970203     O                       PER3                66
340100950622     O*COLLI PISTOLA 87 (EX 88 E 89)
340200970203     O                       TRPI87        Z    106
340300970203     O                       TRPC87        4    113
340400970203     O                       PER34              114
340500931008     O**PESO
340600970203     O                       XKD123        4    124
340700970203     O                       XKE123        4    134
340800970203     O                       XKF123        4    144
340900931008     O**PESO TASSABILE
341000970203     O                       TRPETS        4    155
341100931008     O**VOLUME
341200970203     O                       XKI123        4    165
341300931008     O**SPEDIZIONI
341400970203     O                       XKL123        Z    174
341500931008     O*
341600931011     O****2°RIGA DEL DETTAGLIO TRANSITI = ESPRESSO
341700931008     O**COLLI
341800931008     O          E            DETTRA      1
341900970203     O                                           11 'ESPR'
342000970203     O                       XKA(2)        Z     18
342100931008     O**PESO
342200970203     O                       XKD(2)        4    124
342300970203     O                       XKE(2)        4    134
342400970203     O                       XKF(2)        4    144
342500931008     O**VOLUME
342600970203     O                       XKI(2)        4    165
342700931008     O**SPEDIZIONI
342800970203     O                       XKL(2)        Z    174
342900931011     O*
343000950111     O****3°RIGA DEL DETTAGLIO TRANSITI = DISTRIBUZIONE
343100931008     O**COLLI
343200931008     O          E            DETTRA      1
343300970203     O                                           11 'DIST'
343400970203     O                       XKA(3)        Z     18
343500931008     O**PESO
343600970203     O                       XKD(3)        4    124
343700970203     O                       XKE(3)        4    134
343800970203     O                       XKF(3)        4    144
343900931008     O**VOLUME
344000970203     O                       XKI(3)        4    165
344100931008     O**SPEDIZIONI
344200970203     O                       XKL(3)        Z    174
344300940323     O*
344400950111     O****4°RIGA DEL DETTAGLIO TRANSITI = CORRIERE
344500940323     O**COLLI
344600940323     O          E            DETTRA      1
344700970203     O                                           11 'CORR'
344800970203     O                       XKA(1)        Z     18
344900940323     O**PESO
345000970203     O                       XKD(1)        4    124
345100970203     O                       XKE(1)        4    134
345200970203     O                       XKF(1)        4    144
345300940323     O**VOLUME
345400970203     O                       XKI(1)        4    165
345500940323     O**SPEDIZIONI
345600970203     O                       XKL(1)        Z    174
345700941012      * ------------------?    TOTALI  GENERALI    ?------------------
345800940401     O*TRATTINI DI SEPARAZIONE TRA I TRANSITI ED IL TOTALE GENERALE.
345900941012     O          E            DETTOT      1
346000941012     O                       SK(5)               66
346100941012     O                       SK(5)              132
346200970306     O                       SK(18)             198
346300931008     O****1°RIGA DEL DETTAGLIO TOT.GEN. = TOTALI
346400931008     O**COLLI
346500970203     O          E            DETTOT      1
346600970203     O                                           10 'TOT.GENER.'
346700970203     O                       TKA123        Z     18
346800931008     O*COLLI NO SPUNT. IN PART.
346900970203     O                       TGP88T        Z     58
347000970203     O                       TGC88T        4     65
347100970203     O                       PER3                66
347200950622     O*COLLI PISTOLA 87 (EX 88 E 89)
347300970203     O                       TGP87T        Z    106
347400970203     O                       TGC87T        4    113
347500970203     O                       PER34              114
347600931008     O**PESO
347700970203     O                       TKD123        4    124
347800970203     O                       TKE123        4    134
347900970203     O                       TKF123        4    144
348000931008     O**PESO TASSABILE
348100970203     O                       TGPETS        4    155
348200931008     O**VOLUME
348300970203     O                       TKI123        4    165
348400931008     O**SPEDIZIONI
348500970203     O                       TKL123        Z    174
348600931008     O*
348700931011     O****2°RIGA DEL DETTAGLIO TOT.GEN. = ESPRESSO
348800931008     O**COLLI
348900931008     O          E            DETTOT      1
349000970203     O                                           11 'ESPR'
349100970203     O                       TKA(2)        Z     18
349200931008     O**PESO
349300970203     O                       TKD(2)        4    124
349400970203     O                       TKE(2)        4    134
349500970203     O                       TKF(2)        4    144
349600931008     O**VOLUME
349700970203     O                       TKI(2)        4    165
349800931008     O**SPEDIZIONI
349900970203     O                       TKL(2)        Z    174
350000931008     O*
350100950111     O****3°RIGA DEL DETTAGLIO TOT.GEN. = DISTRIBUZIONE
350200931008     O**COLLI
350300931008     O          E            DETTOT      1
350400970203     O                                           11 'DIST'
350500970203     O                       TKA(3)        Z     18
350600931008     O**PESO
350700970203     O                       TKD(3)        4    124
350800970203     O                       TKE(3)        4    134
350900970203     O                       TKF(3)        4    144
351000931008     O**VOLUME
351100970203     O                       TKI(3)        4    165
351200931008     O**SPEDIZIONI
351300970203     O                       TKL(3)        Z    174
351400940323     O*
351500950111     O****4°RIGA DEL DETTAGLIO TOT.GEN. = CORRIERE
351600940323     O**COLLI
351700940323     O          E            DETTOT      1
351800970203     O                                           11 'CORR'
351900970203     O                       TKA(1)        Z     18
352000940323     O**PESO
352100970203     O                       TKD(1)        4    124
352200970203     O                       TKE(1)        4    134
352300970203     O                       TKF(1)        4    144
352400940323     O**VOLUME
352500970203     O                       TKI(1)        4    165
352600940323     O**SPEDIZIONI
352700970203     O                       TKL(1)        Z    174
352800931008     O*TRATTINI DI SEPARAZIONE DOPO IL TOTALE GENERALE.
352900931008     O          E            DETTOT      1  1
353000931008     O                       SK(5)               66
353100931008     O                       SK(5)              132
353200970306     O                       SK(18)             198
353300931130      *
353400941012     O          E            FINSTA           63
353500931130     O                       SK1(1)              66
353600941012     O          E            FINSTA           64
353700931130     O                       SK1(2)              66
353800931130     O                       SK1(3)             132
353900931130     O                       SK1(4)             198
354000950111     O                       §QTKPM        2     72
354100931130     O                       §3IARR        Z     99
354200941012     O          E            FINSTA           65
354300941012     O                       SK1(7)              66
354400940126      * AVVISO DI FOGLI VIAGGIO APERTI NEL PERIODO
354500941012     O          E            FVAPER      2
354600941012     O                       SK1(5)             115
354700941012     O                       SK1(6)            +  0
354800931008     O****************************************************************
354900931130      *            2         3         4         5         6         7         8
355000940406**   CMD           0123456789012345678901234567890123456789012345678901234567890
355100950209OVRPRTF FILE(PRTF198) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.part.
355200950209')
355300930928**   SK
355400050802XXXXXXXXXXXXXXXXXXXX                                                 1
355500950111   STATISTICA PARTENZE - PARTITO  IL XX/XX/XXXX                      2
355600950111   STATISTICA PARTENZE - PARTITO DAL XX/XX/XXXX  AL XX/XX/XXXX       3
355700970807                      FNLSC1R                             PAG.XXXX   4
355800930928------------------------------------------------------------------   5
355900970502  PUNTO     COLLI  |--- COLLI NON SPUNTATI (PIST.88,89,98,96) ---|
356000970204OPERATIVO            DiskB  % sul    Altri  % sul   Totale  % sul    7
356100970204                           partito         partito         partito   8
356200970131 |----- S P U N T E   P I S T O L A   87 ------| |----- P   E   S    9
356300970204   DiskB  % sul    Altri  % sul   Totale  % sul   FINO 35 OLTRE 35  10
356400970204         partito         partito         partito                    11
356500970306  O -------|     PESO      VOLUME    SPEDIZIONI                     12
356600970305 Q    TOTALE  TASSABILE            Totale  Tot.Da                   13
356700970305                                   Partito Partire                  14
3568009701311234567890 1234567 1234567 123,45% 1234567 123,45% 1234567 123,45%  15
356900970131 1234567 123,45% 1234567 123,45% 1234567 123,45% 1234567,8 1234567  16
357000970305,8 1234567,8 12345678,9 12345,6789 1234567 1234567 1234567 123,45%  17
357100970306--------------------------------------------------                  18
357200931130**
357300940405N.B.
357400940405PESO TASSABILE:  E' CALCOLATO CON UN RAPPORTO PESO/VOLUME  1 MC =
357500940405XX,XXX Q.LI ARROTONDATO AI XXXXXX KG. SUPERIORI, PER LE SOLE SPEDI
357600940405ZIONI CON PESO FINO A KG. 3500, ESCLUSE BOLLE DI RECUPERO E C/SERV
357700940126***   SONO STATI TROVATI ALCUNI FOGLI VIAGGIO APERTI AVENTI DATA P
357800940126ARTENZA COMPRESA NEL PERIODO   ***
357900940406SPEDIZIONI:  DALLA STATISTICA SONO ESLUSE LE BOLLE DI RECUPERO
