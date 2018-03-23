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
010000090605
010100090605     d wdtsp           s              5    dim(10)
010200020418
010300020418     d wc              s              3  0
010400021016
010500021016     d kTbeCod         s                   Like(TbeCod)
010600021016     d kTbeKe1         s                   Like(TbeKe1)
010700050829     d klin            s                   Like(ds1lnp)
010800050829     d kdre            s                   Like(spadre)
010900020418
011000931130      *--DS*---------------------------------------------------------*
011100931130      *
011200931130      * DS PER TESTARE QUANDO CAMBIA BOLLA (SI VALORIZZA LEGGENDO I SEGNACOLLI).
011300930930     D                 DS
011400950111     D  BLTAAS                 1      4  0
011500950111     D  BLTLNP                 5      7  0
011600950111     D  BLTNRS                 8      9  0
011700950111     D  BLTNSP                10     16  0
011800950111     D  DSTXT                  1     16  0
011900950111     D                 DS
012000950111     D  BTTAAS                 1      4  0
012100950111     D  BTTLNP                 5      7  0
012200950111     D  BTTNRS                 8      9  0
012300950111     D  BTTNSP                10     16  0
012400950111     D  DSTXT1                 1     16  0
012500931130      *
012600931130      * DS PER TESTARE SE BOLLETTATO IN PERIODO O IN PERIODO PRECEDENTE.
012700930930     D                 DS
012800941012     D  BLPAAS                 1      4  0
012900941012     D  BLPMGS                 5      8  0
013000941012     D  BLPDS                  1      8  0
013100040921      *
013200040921     D WLBDAT          DS                  INZ
013300040921     D  G02DAT                 1      8  0
013400040921     D  G02INV                 9     16  0
013500040921     D  G02ERR                17     17
013600040921     D  G02TGI                18     22  0
013700921222     D DS3A          E DS
013800950111     D* N.B.: SE VARIA LA LUNGHEZZA DI DS3A, MODIF.LA SCHIERA D3A
013900930927     D DS3I          E DS
014000921222     D DS3L          E DS
014100040921     D*DS5C          E DS
014200970707     D DS5P          E DS
014300950111     D DSQT1         E DS
014400000831     D OG143         E DS
014500950210     D DSLS01        E DS
014600970204     D  ANNODA                41     44  0
014700970204     D  MESEDA                45     46  0
014800970204     D  $GI                   47     48  0
014900920930     D CNCR80        E DS
015000920930     D KPJBA         E DS
015100920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015200950209     D* DS PER TRUL06R - CARICAMENTO £X
015300950209     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015400950314     D  T06                    1     90  0
015500950314     D                                     DIM(30)
015600970129      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015700970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
015800000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
015900000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
016000021016      * Ds date statistiche
016100021016     d dSdf          e ds
016200070523     D* DS PER LETTURA TABELLA "VPO" record DECO
016300070523     D Dvpodeco      E DS
016400090605
016500090605     d ds5e          e ds
016600020813      *
016700020813      * DEFINIZIONE CAMPI
016800020813      *
016900020813     D SAVRIS          S                   LIKE(DS1RIS) inz
017000020813     D WCAA            S                   LIKE(ANMCAA) inz
017100021220     D WVOLUM          S                   LIKE(VOLUM)  inz
017200021220     D WPESCOL         S                   LIKE(PESCOL) inz
017300020813     D KDUC            S                   LIKE(BLPDUC) inz
017400020813     D KDBR            S                   LIKE(BLPDBR) inz
017500070523     D KDrt            S                   LIKE(BLPDrt) inz
017600020813     D $TFA            S                   LIKE(BLPTFA) inz
017700020813     D W0010           S                   LIKE(SPASFC) inz
017800020813     D W0020           S                   LIKE(SPASFC) inz
017900020813     D W0030           S                   LIKE(SPASFC) inz
018000020813     D KNPG            S                   LIKE(BRVNPG) inz
018100020813     D SAVLNP          S                   LIKE(BLPLNP) inz
018200020813     D*
018300020813     D KCOD            S                   LIKE(TBLCOD) inz
018400020813     D KKEY            S                   LIKE(TBLKEY) inz
018500020813     D COMFIL          S                   LIKE(ORGFIL) inz
018600020813     D FLDTXT          S                   LIKE(DSTXT)  inz
018700090415     D ktrc            S                   LIKE(statrc) inz('P')
018800090415     D kfil            S                   LIKE(stafil) inz(000)
018900021213      *
019000021213     d Volum           s             12  4 inz
019100021213     d PesCol          s              8  1 inz
019200021213     d PesTax          s              8  0 inz
019300021213     d PesTas          s              9  1 inz
019400030213     d*** WsiVOL          s              1    inz
019500030213     d*** WsiPES          s              1    inz
019600021213     d Wvolu           s                   like(BLPvlf) inz
019700021213     d Wpeso           s                   like(BLPpkf) inz
019800931130      *---------------------------------------------------------------*
019900931130      *    RIEPILOGO INDICATORI
020000931130      *---------------------------------------------------------------*
020100931130      * 03    - IMMESSA DATA AL
020200970129      * 05    - ON,CLIENTE DISK B
020300931130      * 04    - ON,SE BOLLA=C/SERVIZI; OFF SE BOLLA DI FILIALE.
020400931130      * 06    - ON,SE BOLLA FINO A 35 Q. OFF SE OLTRE 35 Q.
020500931130      * 07    - ON,SE BOLLA BOLLETTATA IN PERIODO PRECEDENTE.(PER CICLO SPEDIZ.)
020600931130      * 30/32 - DI COMODO
020700940328      * 36    - ON,SE NON SI VUOLE IL TABULATO STAMPATO
020800931130      *---------------------------------------------------------------*
020900000000     C     *ENTRY        PLIST
021000000000     C                   PARM                    KPJBA
021100950210     C                   MOVEL     KPJBU         DSLS01
021200950210      *
021300931130      *
021400931130      *---------------------------------------------------------------*
021500931130      * LETTURA FILE SEGNACOLLI (COLLI).
021600930924     C     KBLT30        KLIST
021700930924     C                   KFLD                    COMFIL
021800950210     C                   KFLD                    DS1ADA
021900970129     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
022000970129     C     KBLP6         KLIST
022100970129     C                   KFLD                    COMFIL
022200970129     C                   KFLD                    KDUC
022300970204     C     KBLP6A        KLIST
022400970204     C                   KFLD                    COMFIL
022500970204     C                   KFLD                    KDUC
022600970204     C                   KFLD                    KDBR
022700970129     C     KBLP6B        KLIST
022800970129     C                   KFLD                    COMFIL
022900970129     C                   KFLD                    BLPDUC
023000970204     C     KBLP6C        KLIST
023100970204     C                   KFLD                    COMFIL
023200970204     C                   KFLD                    BLPDUC
023300970204     C                   KFLD                    BLPDBR
023400070523     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
023500070523     C     KBLP5         KLIST
023600070523     C                   KFLD                    COMFIL
023700070523     C                   KFLD                    Kduc
023800070523     C     KBLP5A        KLIST
023900070523     C                   KFLD                    COMFIL
024000070523     C                   KFLD                    Kduc
024100070523     C                   KFLD                    KDrt
024200070523     C     KBLP5B        KLIST
024300070523     C                   KFLD                    COMFIL
024400070523     C                   KFLD                    BLPDUC
024500931130      * LETTURA FILE BOLLE. (PER RISALIRE DAL COLLO ALLA BOLLA)
024600950111     C     KBLP01        KLIST
024700930924     C                   KFLD                    BLTAAS
024800931008     C                   KFLD                    BLTLNP
024900930924     C                   KFLD                    BLTNRS
025000930924     C                   KFLD                    BLTNSP
025100020823     C     KBTP11        KLIST
025200020823     C                   KFLD                    BTTFLP
025300020823     C                   KFLD                    BTTAAS
025400950111     C                   KFLD                    BTTLNP
025500950111     C                   KFLD                    BTTNRS
025600950111     C                   KFLD                    BTTNSP
025700970203     C     FNSPAK        KLIST
025800970128     C                   KFLD                    SIMFEL
025900000614     C                   KFLD                    LIN(XX)
026000970128     C                   KFLD                    DS1ADA
026100970204     C*
026200970204     C     KCLN          KLIST
026300970204     C                   KFLD                    SIMFEL
026400970204     C                   KFLD                    $TFA
026500970204     C                   KFLD                    ANNODA
026600970204     C                   KFLD                    MESEDA
026700970207     C*
026800970207     C     KANM          KLIST
026900970207     C                   KFLD                    WCAA
027000990518     C                   KFLD                    DS1ADA
027100000204     C     KBRV          KLIST
027200970505     C                   KFLD                    KNPG
027300970505     C                   KFLD                    ANMFLS
027400970505     C                   KFLD                    ANMLNA
027500970505     C                   KFLD                    ANMNRS
027600970505     C                   KFLD                    ANMSCN
027700970505     C                   KFLD                    SIMFEL
027800020823     C     KDTA          KLIST
027900020823     C                   KFLD                    SIMFEL
028000020823     C                   KFLD                    DS1ADA
028100021016
028200021016     c     Ktbe          Klist
028300021016     c                   Kfld                    kTbeCod
028400021016     c                   Kfld                    kTbeKe1
028500050829     c     KSPA2         Klist
028600050829     c                   Kfld                    DS1FEL
028700050829     c                   Kfld                    KLIn
028800050829     c     KSPA          Klist
028900050829     c                   Kfld                    DS1FEL
029000050829     c                   Kfld                    KLIn
029100050829     c                   Kfld                    Kdre
029200090415     c     KSTA          Klist
029300090415     c                   Kfld                    Ktrc
029400090415     c                   Kfld                    DS1FEL
029500090415     c                   Kfld                    Kfil
029600090415     c                   Kfld                    ds1ada
029700931130      *---------------------------------------------------------------*
029800931130      * ESEGUO OPERAZ. DA FARE UNA VOLTA SOLA, ALL'INIZIO
029900930927     C                   EXSR      INIZIO
030000950209      *
030100950209      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
030200950209     C                   EXSR      COMUNE
030300970129     C*
030400970129     C* CALL PGM PER CHIUDERE FILES
030500970129     C     DS1RIS        IFNE      'R'
030600970129     C                   MOVEL     'C'           D98TLA
030700970925     C                   CALL      'TNSD99R'
030800970925     C                   PARM                    DS99
030900970203     C                   END
031000931130      *
031100920930     C                   SETON                                        LR
031200950209      *
031300020212     C*****************************************************************
031400020212     C*  PARTE COMUNE STAMPE GIORNALIERE
031500020212     C*
031600020212     C*  . REPERIMENTO 'SIMFEL'
031700020212     C*  . CARICAMENTO '£1'
031800020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
031900020212     C*  . CODA DI STAMPA ('£OUTQ')
032000020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
032100020212     C*  . MODULO PER LA STAMPA ('£MOD')
032200020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
032300020212     C*****************************************************************
032400020212     C     COMUNE        BEGSR
032500020212     C*
032600020212     C*****************************************************************
032700020212     C*       RIEPILOGO INDICATORI:
032800020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
032900020212     C* 11    -           STAMPA PER 1L
033000020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
033100020212     C*****************************************************************
033200020813     ***C                   Z-ADD     1             CODUT
033300020212     C*
033400020212     C* Reperimento parametri aziendali
033500020212     C                   CALL      'X§PARUT'
033600020212     C                   PARM                    UT§DSE
033700020212     C                   MOVEL     RAGUT         RSUT             20
033800020212     C                   MOVEL     REC80         CNCR80
033900020212     C*
034000020212     C* Chiave di accesso per il file tabelle
034100020212     C     KTAB1         KLIST
034200020212     C                   KFLD                    CODUT
034300020212     C                   KFLD                    KCOD
034400020212     C                   KFLD                    KKEY
034500020212     C     KTAB2         KLIST
034600020212     C                   KFLD                    CODUT
034700020212     C                   KFLD                    KCOD
034800020212     C*
034900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
035000020212     C*
035100020212     C* DETERMINO DATA E ORA DI ESECUZIONE
035200020212     C                   TIME                    WTIME            14 0
035300020212     C                   MOVE      WTIME         WDATE             8 0
035400020212     C                   MOVEL     WTIME         UTIME             6 0
035500040921     c*
035600040921     C                   CLEAR                   WLBDAT
035700040921     C                   MOVE      Wtime         G02DAT
035800040921     C                   CALL      'XSRDA8'
035900040921     C                   PARM                    WLBDAT
036000040921     C                   Z-ADD     G02INV        dateu             8 0
036100020212     C*
036200020418     C* CARICO TABELLA '£1'
036300020212     C                   CLEAR                   KPJBU
036400020212     C                   CLEAR                   DSUL06
036500020212     C                   MOVE      '£1'          D06COD
036600020212     C                   MOVEL     SIMFEL        D06KEY
036700050802     C                   MOVE      'S'           D06KEY
036800050802     C                   MOVEL     ds1ada        D06drf
036900020212     C                   MOVEL     DSUL06        KPJBU
037000020212     C                   CALL      'TRUL06R'
037100020212     C                   PARM                    KPJBA
037200020212     C                   MOVEL     KPJBU         DSUL06
037300020418     c                   movea     t06           sav_t06
037400020418
037500020418      * se in arrivo carico la £6
037600020418 b1  c                   if        ds1tbo = 'A'
037700020418     c                   clear                   wc
037800020418 b2  c                   do        30            wa
037900020422     c                   if        sav_t06(wa) = *zeros
038000020422     c                   leave
038100020422     c                   endif
038200020418     c                   clear                   kpjbu
038300020418     c                   clear                   dsul06
038400050802     C                   MOVEL     ds1ada        D06drf
038500020418     c                   eval      d06cod = '£6'
038600020418     c                   movel     sav_t06(wa)   d06key
038700050802     C                   MOVE      'S'           D06KEY
038800020418     c                   eval      d06esc = 'S'
038900020419     c                   movel     dsul06        kpjbu
039000020418     c                   call      'TRUL06R'
039100020418     c                   parm                    kpjba
039200020418     c                   movel     kpjbu         dsul06
039300020418 b3  c                   do        30            wb
039400020418     c                   if        t06(wb) = *zeros
039500020418     c                   leave
039600020418     c                   endif
039700020418     c                   add       1             wc
039800020418     c                   z-add     t06(wb)       l6(wc)
039900020418 e3  c                   enddo
040000020418 e2  c                   enddo
040100020212     C*
040200020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
040300020212     C                   Z-ADD     *ZERO         WB
040400020418 b2  C                   DO        29            WA
040500020418     C     sav_T06(WA)   LOOKUP    L6                                     30
040600020212     C  N30              ADD       1             WB
040700020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
040800020418 e2  C                   ENDDO
040900020418 x1  C                   ELSE
041000020418     C                   MOVEA     sav_T06       L1
041100020418 e1  C                   ENDIF
041200020212     C*
041300070103     C* ESCLUDO DA £1 LINEE NON consentite
041400020212     C                   MOVEA     L1            LIN
041500020212     C                   CLEAR                   L1
041600020212     C                   Z-ADD     *ZERO         WB                3 0
041700020212     C     1             DO        30            WPT               3 0
041800020212     C     LIN(WPT)      IFNE      0
041900020212     C                   CLEAR                   FNLSC4
042000020212     C                   Z-ADD     LIN(WPT)      IC4LIN
042100020212     C                   MOVE      'B'           IC4FLG
042200020212     C                   MOVEL     FNLSC4        KPJBU
042300020212     C                   CALL      'FNLSC4R1'
042400020212     C                   PARM                    KPJBA
042500050802     C                   PARM                    ds1ada
042600020212     C                   MOVEL     KPJBU         FNLSC4
042700020212     C     OC4ESE        IFEQ      'S'
042800020212     C                   ADD       1             WB
042900020212     C                   MOVE      LIN(WPT)      L1(WB)
043000020212     C                   ENDIF
043100020212     C                   ENDIF
043200020212     C                   ENDDO
043300070103     C*
043400070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
043500070103     C                   Z-ADD     *ZERO         WB                3 0
043600070103     C                   DO        29            WA                3 0
043700070103     C     L1(WA)        IFNE      *ZEROS
043800070103     C     L1(WA)        ANDNE     SIMFEL
043900070103     C                   ADD       1             WB
044000070103     C                   Z-ADD     L1(WA)        LV2(WB)
044100070103     C                   ENDIF
044200070103     C                   ENDDO
044300020212     C*
044400020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
044500020212     C*
044600020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
044700020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
044800020212     C* la stampa e' lanciata da menu
044900020212     C                   MOVEL     '3L'          KCOD
045000020212     C                   MOVEL     DS1PGM        KKEY
045100020212     C                   MOVE      DS1FPG        KKEY
045200020212     C     KTAB1         CHAIN     TABEL00F                           30
045300020212     C  N30              MOVEL     TBLUNI        DS3L
045400020212     C   30              MOVEL     'F'           §3LD1L
045500020212     C   30              MOVEL     ' '           §3LD2L
045600020212     C   30              MOVEL     '*STD    '    §3LDST
045700020212     C*
045800020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
045900020212     C                   MOVEL     §3LDST        £MOD             10
046000020212     C                   CLEAR                   LIN
046100020212     C*
046200020212     C* Verifico chi ha lanciato la stampa
046300020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
046400020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
046500020212     C* DS1FGS<>0  --->  si proviene da menu
046600020212     C     DS1FGS        IFEQ      0
046700020212     C*
046800020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
046900020212     C* lancio, voglio la stampa;  invece
047000020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
047100020212     C*
047200020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
047300020212     C* creare stampa per 1°lvl. (se stessa)
047400020212     C     DS1LNP        IFEQ      SIMFEL
047500020212     C*
047600020212     C* Il flag §3LD1L ha i seguenti significati:
047700020212     C*    ' ' = tutti i dati
047800020212     C*    '1' = solo filiale di 1' livello
047900020212     C     §3LD1L        IFEQ      '1'
048000020212     C                   SETON                                        11
048100020212     C                   END
048200020212     C*
048300020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
048400020212     C* nella stampa
048500020212     C*
048600020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
048700020212     C  N11              MOVEA     L1            LIN
048800020212     C   11              MOVEA     SIMFEL        LIN(1)
048900020212     C*
049000020212     C* IMPOSTO LA STAMPANTE:
049100020212     C* STAMPANTE TABULATI
049200020212     C                   MOVEL     '5P'          KCOD
049300020212     C                   MOVEL     'T'           W004A             4
049400020212     C                   MOVE      LIN(1)        W004A
049500020212     C                   MOVEL(P)  W004A         KKEY
049600020212     C     KTAB1         CHAIN     TABEL00F                           99
049700020212     C     *IN99         IFEQ      *OFF
049800020212     C                   MOVEL     TBLUNI        DS5P
049900020212     C                   MOVEL     §5POTQ        £OUTQ            10
050000020212     C                   END
050100020212     C*
050200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
050300020212     C                   EXSR      LEGGI
050400020212     C*
050500020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
050600020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
050700020212     C                   ELSE
050800020212     C*
050900020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
051000020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
051100020212     C*
051200020212     C* Il significato del fla §3LD2L e' il seguente:
051300020212     C*    ' ' = tutti i dati
051400020212     C*    'F' = per ogni 2' livello
051500020212     C     §3LD2L        IFEQ      'F'
051600020212     C                   SETON                                        14
051700020212     C                   ENDIF
051800020212     C*
051900020212     C                   Z-ADD     1             WB
052000020212     C                   MOVEA     *ZEROS        LIN
052100020212     C  N14              MOVEA     L1            LIN
052200020212     C*
052300020212     C* Lancio per tutti i 2° lvl
052400020212     C     DS1LNP        IFEQ      0
052500020212     C*
052600020212     C* LV2 contiene tutte le filiali di 2° lvl
052700020212     C     LV2(WB)       DOWNE     0
052800020212      *
052900020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
053000020212     C                   CLEAR                   FNLSC4
053100020212     C                   Z-ADD     LV2(WB)       IC4LIN
053200020212     C                   MOVEL     FNLSC4        KPJBU
053300020212     C                   CALL      'FNLSC4R1'
053400020212     C                   PARM                    KPJBA
053500050802     C                   PARM                    ds1ada
053600050802     C                   MOVEL     KPJBU         FNLSC4
053700020212      *
053800020212     C     OC4PT1        IFEQ      'S'
053900020212     C     §3LPT         ANDEQ     'N'
054000020212     C                   GOTO      NOLEGG
054100020212     C                   ENDIF
054200020212      *
054300020212     C     OC4EXP        IFEQ      'S'
054400020212     C     §3LEXP        ANDEQ     'N'
054500020212     C                   GOTO      NOLEGG
054600020212     C                   ENDIF
054700020212      *
054800020212     C     OC4DPD        IFEQ      'S'
054900020212     C     §3LDPD        ANDEQ     'N'
055000020212     C                   GOTO      NOLEGG
055100020212     C                   ENDIF
055200020212      *
055300020212     C     OC4BAR        IFEQ      'S'
055400020212     C     §3LBAR        ANDEQ     'N'
055500020212     C                   GOTO      NOLEGG
055600020212     C                   ENDIF
055700020212      *
055800020212     C     OC4FED        IFEQ      'S'
055900020212     C     §3LFED        ANDEQ     'N'
056000020212     C                   GOTO      NOLEGG
056100020212     C                   ENDIF
056200020212      *
056300020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
056400020212     C*  dati degli altri 2° lvl
056500020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
056600020212     C   14              MOVEL     LV2(WB)       LIN(1)
056700020212     C*
056800020212     C* IMPOSTO LA STAMPANTE
056900020212     C* STAMPANTE TABULATI
057000020212     C                   MOVEL     '5P'          KCOD
057100020212     C                   MOVEL     'T'           W004A             4
057200020212     C                   MOVE      LV2(WB)       W004A
057300020212     C                   MOVEL(P)  W004A         KKEY
057400020212     C     KTAB1         CHAIN     TABEL00F                           99
057500020212     C     *IN99         IFEQ      *OFF
057600020212     C                   MOVEL     TBLUNI        DS5P
057700020212     C                   MOVEL     §5POTQ        £OUTQ            10
057800020212     C                   END
057900020212     C*
058000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
058100020212     C                   EXSR      LEGGI
058200020212     C*
058300020212     C     NOLEGG        TAG
058400020212     C*
058500020212     C                   ADD       1             WB
058600020212     C                   ENDDO
058700020212     C*
058800020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
058900020212     C                   ELSE
059000020212     C*
059100020212     C* Lancio solo per la 2° lvl richiesta
059200020212     C   14              MOVEL     DS1LNP        LIN(1)
059300020212     C*
059400020212     C* STAMPANTE
059500020212     C* STAMPANTE TABULATI
059600020212     C                   MOVEL     '5P'          KCOD
059700020212     C                   MOVEL     'T'           W004A             4
059800020212     C                   MOVE      DS1LNP        W004A
059900020212     C                   MOVEL(P)  W004A         KKEY
060000020212     C     KTAB1         CHAIN     TABEL00F                           99
060100020212     C     *IN99         IFEQ      *OFF
060200020212     C                   MOVEL     TBLUNI        DS5P
060300020212     C                   MOVEL     §5POTQ        £OUTQ            10
060400020212     C                   END
060500020212     C*
060600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
060700020212     C                   EXSR      LEGGI
060800020212     C*
060900020212     C                   ENDIF
061000020212     C                   ENDIF
061100020212     C*
061200020212     C                   ELSE
061300020212     C*
061400020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
061500020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
061600020212     C     DS1LNP        IFEQ      0
061700020212     C                   MOVEA     L1            LIN
061800020212     C* ...altrimenti elaboro solo se stessa
061900020212     C                   ELSE
062000020212     C                   MOVEL     DS1LNP        LIN(1)
062100020212     C                   ENDIF
062200020212     C*
062300020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
062400020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
062500020212     C* PRENDO QUELLA PRESENTE IN KPJBA
062600020212     C     DS1FGS        IFEQ      SIMFEL
062700020212     C                   MOVEL     KQOTD         £OUTQ
062800030627     C**!!!              MOVEL     KQOTD         £OUTQF
062900020212     C                   ELSE
063000020212     C* STAMPANTE TABULATI
063100020212     C                   MOVEL     '5P'          KCOD
063200020212     C                   MOVEL     'T'           W004A             4
063300020212     C                   MOVE      DS1FGS        W004A
063400020212     C                   MOVEL(P)  W004A         KKEY
063500020212     C     KTAB1         CHAIN     TABEL00F                           99
063600020212     C     *IN99         IFEQ      *OFF
063700020212     C                   MOVEL     TBLUNI        DS5P
063800020212     C                   MOVEL     §5POTQ        £OUTQ            10
063900020212     C                   END
064000020212     C                   ENDIF
064100020212     C*
064200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
064300020212     C                   EXSR      LEGGI
064400020212     C                   ENDIF
064500020212     C*
064600020212     C                   ENDSR
064700931130      *****************************************************************
064800931130      * SUBROUTINE INIZIO: OPERAZIONI DA FARE UNA VOLTA SOLA ALL'INIZIO
064900931130      ****************************************************************
065000930927     C     INIZIO        BEGSR
065100950209      *
065200950209     C                   Z-ADD     1             CODUT
065300940104      *
065400941012      *  Stampa o ristampa ?
065500941012     C                   MOVE      'RISTAMPA'    STRIP             8
065600950404     C     DS1RIS        IFEQ      'S'
065700941012     C                   MOVE      'STAMPA  '    STRIP
065800941012     C                   ENDIF
065900941012      *
066000941012      * Scrivo, in stampa, se selezionata filiale o area
066100941012     C                   MOVE      *BLANKS       STRIP1            8
066200950210     C     DS1LNP        IFEQ      *ZERO
066300941012     C                   MOVEL     'AREA'        STRIP1
066400950210     C                   MOVE      DS1FEL        STRIP1
066500941012     C                   ELSE
066600960925     C                   MOVEL     'P.O.'        STRIP1
066700950210     C                   MOVE      DS1LNP        STRIP1
066800941012     C                   ENDIF
066900940429      *
067000941012      * Per sicurezza, se la data finale e' nulla metto quella iniziale
067100950210     C     DS1AAL        IFEQ      *ZERO
067200950210     C                   Z-ADD     DS1ADA        DS1AAL
067300970130     C                   Z-ADD     DS1GDA        DS1GAL
067400941012     C                   ENDIF
067500931130      *
067600931130      * TESTO SE E' STATA IMMESSA 'DATA A' ED E' <> DA 'DATA DA', SETON 03.
067700950210     C     DS1AAL        IFNE      0
067800950210     C     DS1AAL        ANDNE     DS1ADA
067900930927     C                   SETON                                        03
068000930927     C                   ENDIF
068100940509      *
068200941012      * Accendo il 36 se non desidero la stampa
068300950210     C     DS1RIS        COMP      *BLANKS                                36
068400940509      *
068500950111      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
068600950209     C                   MOVEL     'QT'          KCOD
068700950209     C                   MOVEL     '1       '    KKEY
068800950209     C     KTAB1         CHAIN     TABEL00F                           32
068900950111     C  N32              MOVEL     TBLUNI        DSQT1
069000950111     C   32              CLEAR                   DSQT1
069100931130      *
069200931130      * REPERIMENTO TABELLA VARIABILI PGM
069300950209     C                   MOVEL     '3I'          KCOD
069400950209     C                   MOVEL     '1       '    KKEY
069500950209     C     KTAB1         CHAIN     TABEL00F                           32
069600930927     C  N32              MOVEL     TBLUNI        DS3I
069700930927     C   32              Z-ADD     20            §3IARR
069800950111      *
069900950111      * METTO IN SCHIERA LA TABELLA 3A PER EVITARE CHAIN SUCCESSIVE
070000950111     C                   Z-ADD     *ZERO         XX
070100950209     C                   MOVE      '3A'          KCOD
070200950209     C     KTAB2         SETLL     TABEL00F
070300950209     C     KTAB2         READE     TABEL00F                               32
070400950111     C     *IN32         DOWEQ     *OFF
070500950111     C                   ADD       1             XX
070600950209     C                   MOVEL     TBLKEY        E3A(XX)
070700950111     C                   MOVEL     TBLUNI        D3A(XX)
070800950209     C     KTAB2         READE     TABEL00F                               32
070900950111     C                   ENDDO
071000070523     c
071100070523     c* Carico la tabella VPO per vedere da quando cambia il calcolo
071200070523     c*  della affidabilità partenza
071300070523     C                   MOVEL     'VPO'         kTBECOD
071400070523     C                   MOVEL(P)  'DECO'        kTBEKE1
071500070523     C     KTBE          CHAIN(N)  TNTBE01L                           21
071600070523     C  N21              MOVEL     TBEUNI        DVPODECO
071700070523     C*
071800090605      * carico le descrizioni fisse dei tipi servizio
071900090605      * chiodo l'indice di schiera per ogni tipo servizio
072000090605      * 1=C - 2=E - 3=D
072100090605     c                   clear                   ds5e
072200090605     c                   clear                   wdtsp
072300090605     c                   eval      kcod = '5E'
072400090605     c     ktab2         setll     tabel00f
072500090605     c                   do        *hival
072600090605     c     ktab2         reade(n)  tabel00f
072700090605     c                   if        %eof(tabel00f)
072800090605     c                   leave
072900090605     c                   endif
073000090605     c                   eval      ds5e = tbluni
073100090605     c                   if        tblkey = 'C'
073200090605     c                   eval      wdtsp(01) = §5ed05
073300090605     c                   endif
073400090605     c                   if        tblkey = 'D'
073500090605     c                   eval      wdtsp(03) = §5ed05
073600090605     c                   endif
073700090605     c                   if        tblkey = 'E'
073800090605     c                   eval      wdtsp(02) = §5ed05
073900090605     c                   endif
074000090605     c                   enddo
074100090605
074200930927     C                   ENDSR
074300930927     C*****************************************************************
074400930924     C* SUBROUTINE LEGGI: CICLO PRINCIPALE
074500930924     C*****************************************************************
074600920930     C     LEGGI         BEGSR
074700000615     C                   CLEAR                   SAVRIS
074800941012      *
074900941012      * Se sono nelle stampe giornaliere non voglio diciture
075000950210     C     DS1FGS        IFEQ      *ZERO
075100941012     C     SIMFEL        ANDNE     *ZERO
075200941012     C                   MOVE      *BLANKS       STRIP
075300941012     C                   MOVE      *BLANKS       STRIP1
075400941012     C                   ENDIF
075500941012      *
075600970129      * Cancello dal file statistiche i record della giornata che vado
075700000614      * a elabor. ELABORO SEMPRE TUTATA LA L1 E STAMPO LIN
075800000614      * D'ORA IN AVANTI IL PGM SARA' SEMRPE CHIAMATO SEPARATAMENTE
075900000614      * PERELABORARE E PER STAMPARE
076000950210     C     DS1RIS        IFNE      'R'
076100000614     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
076200000614     C                   CLEAR                   KPJBU
076300000614     C                   CLEAR                   DSUL06
076400000614     C                   MOVE      '£1'          D06COD
076500000614     C                   MOVEL     SIMFEL        D06KEY
076600050802     C                   MOVE      'S'           D06KEY
076700050802     C                   MOVEL     ds1ada        D06drf
076800000614     C                   MOVEL     DSUL06        KPJBU
076900000614     C                   CALL      'TRUL06R'
077000000614     C                   PARM                    KPJBA
077100000614     C                   MOVEL     KPJBU         DSUL06
077200000615     C**
077300000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
077400000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
077500000615     C*  CHE USO ALLA FINE
077600000615     C     DS1RIS        IFNE      ' '
077700000615     C                   MOVEA     LIN           SVLIN
077800000615     C                   MOVEL     DS1RIS        SAVRIS
077900000615     C                   CLEAR                   DS1RIS
078000000615     C                   ENDIF
078100000615     C*
078200000615     C                   MOVEA     T06           LIN
078300941012      *
078400941012     C                   Z-ADD     1             XX                3 0
078500000614     C     LIN(XX)       DOWGT     *ZERO
078600970129      *
078700970129     C     FNSPAK        SETLL     FNSPA01L
078800970129     C     FNSPAK        READE     FNSPA01L                               99
078900940509     C*
079000940509     C     *IN99         DOWEQ     *OFF
079100970128     C                   DELETE    FNSPA000
079200970128     C     FNSPAK        READE     FNSPA01L                               99
079300941012     C                   ENDDO
079400940509      *
079500941012     C                   ADD       1             XX
079600941012     C                   ENDDO
079700941012     C                   ENDIF
079800950209      *
079900950209      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
080000950209     C                   MOVEA     £MOD          CMD(33)
080100950209     C                   MOVEA     £OUTQ         CMD(51)
080200931130      *
080300931130      * SE NON VA BENE L'OVERRIDE, IL PGM VA A FINE.
080400950209     C                   Z-ADD     82            LUNG             15 5
080500950209     C                   MOVEL     *BLANKS       COMMAN           82
080600931130     C                   MOVEA     CMD           COMMAN
080700950209     C                   CALL      'QCMDEXC'                            99
080800920930     C                   PARM                    COMMAN
080900920930     C                   PARM                    LUNG
081000940509     C   99              GOTO      ENDLEG
081100931130      *
081200020813     C* CONTATORE 'XD': USATO PER IL CONTEGGIO FILIALI PER SALTO PAGINA.
081300020813     C                   Z-ADD     1             XC                3 0
081400020813     C                   Z-ADD     0             XD                3 0
081500941012     C  N36              OPEN      PRTF198
081600931013     C****STAMPA TESTATA
081700941012     C  N36              EXCEPT    TESTA
081800970226     C                   SETOFF                                       OF
081900000615     C*
082000000615     C* SE CLEARATO DS1RIS ACCENDO INDICATORE
082100000615     C     DS1RIS        IFEQ      ' '
082200000615     C                   SETON                                        36
082300000615     C                   ENDIF
082400940509     C* SE IN LIN,1 C'E' IL 1°LVL, (08) ELABORO I TRANSITI.
082500940509     C     SIMFEL        LOOKUP    LIN                                    08
082600950403     C*
082700970228     C                   CLEAR                   WDIS
082800970505     C                   CLEAR                   WDIS88
082900970228     C*
083000950403     C     *IN08         IFEQ      *ON
083100970228     C     DS1RIS        ANDNE     'R'
083200950403     C*
083300990517     C* SOMMO LE ANOMALIE DI TIPO "056-DISGUIDI" AI TRANSITI
083400990517     C                   Z-ADD     56            WCAA
083500990518     C     KANM          SETLL     FNANM000
083600990518     C     KANM          READE     FNANM000                               32
083700950403     C*
083800950403     C     *IN32         DOWEQ     *OFF
083900021023      * Deve elaborare solo le anomalie con ANMFLE della L1
084000021023     c     AnmFle        Lookup    L1                                     99
084100021023     c                   If        Not *In99
084200021023     c                   GoTo      Rileggi
084300021023     c                   EndIf
084400970305     C     ANMAIE        IFEQ      'E'
084500950403     C                   ADD       1             WDIS              6 0
084600970505     C* CERCO LA RELATIVA SPUNTA PARTENZA
084700970505     C                   Z-ADD     1             KNPG
084800070112     C     KBRV          CHAIN     FNBRV09L                           32
084900970505     C  N32BRVNPS        IFEQ      88
085000970505     C     BRVNPS        OREQ      89
085100970505     C     BRVNPS        OREQ      98
085200970505     C     BRVNPS        OREQ      96
085300970502     C                   ADD       1             WDIS88            6 0
085400970502     C                   ENDIF
085500970505     C                   ENDIF
085600950403     C*
085700021023     c     Rileggi       Tag
085800021023
085900990518     C     KANM          READE     FNANM000                               32
086000950403     C                   ENDDO
086100950403     C                   ENDIF
086200950403     C*
086300950403     C*
086400941012      * Se richiesta 'S'=Stampa elaboro gli archivi, altrimenti leggo il file
086500970129      * statistiche (FNSPA00F) e lo stampo direttamente
086600950210     C     DS1RIS        IFNE      'R'
086700970204     C* VERIFICO SU CALENDARIO SE GIORNO FESTIVO E LO MEMORIZZO
086800970204     C* (MI SERVE PER SAPERE IL TIPO RECORD DA MEMORIZZARE IN FNSPA)
086900970204     C                   CLEAR                   $TFA
087000970204     C                   CLEAR                   WFES
087100970204     C     KCLN          CHAIN     AZCLN01L                           32
087200970204     C     *IN32         IFEQ      *OFF
087300970204     C                   MOVEA     CLNMAT        GME
087400970204     C                   MOVE      GME($GI)      WFES              1
087500970204     C                   END
087600930924     C*****
087700930924     C*CICLO PER TUTTE LE FILIALI (LINEE DI PARTENZA)
087800020813     C     LIN(XC)       DOWNE     0
087900931011     C*
088000020813     C                   ADD       1             XD
088100930924     C*VALORIZZO IL CAMPO CHIAVE ...LINEA DI PARTENZA
088200020813     C                   MOVEL     LIN(XC)       COMFIL
088300930928     C* REPERIMENTO DECODIFICA LINEA PARTENZA
088400930928     C     COMFIL        CHAIN     AZORG                              32
088500970203     C     *IN32         IFEQ      *OFF
088600970203     C     ORGDE5        IFNE      *BLANKS
088700970203     C                   MOVEL     ORGDE5        DESLNP
088800970203     C                   ELSE
088900970203     C                   MOVEL     ORGDES        DESLNP           10
089000970203     C                   END
089100970203     C                   ELSE
089200970203     C                   MOVEL     *BLANKS       DESLNP
089300970203     C                   END
089400931011     C*
089500930924     C**ELABORAZIONE FILIALI (LNP) CON MEMORIZZAZIONE DEI TOTALI
089600000831     C                   EXSR      ELALNP
089700000831     C**ELABORAZIONE ANOMALIE 105 PER LA LINEA INTERESSATA
089800000831     C                   EXSR      ELA105
089900930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
090000930924     C                   EXSR      TOTLNP
090100020813     C                   ADD       1             XC
090200930924     C                   ENDDO
090300930924     C*****
090400930924     C**CALCOLO % STAMPA DEI C/SERVIZI (CONTO SERVIZI)
090500930924     C                   EXSR      TOTCS
090600931013     C**CALCOLO % E STAMPA AREA(TUTTE LE FILIALI + C/SERVIZI).
090700930924     C                   EXSR      TOTARE
090800931006     C** PER 08 ELABORO I TRANSITI
090900931006     C   08              EXSR      ELATRA
091000930924     C**CALCOLI % E STAMPA DEI TRANSITI
091100931006     C   08              EXSR      TOTTRA
091200940103      *
091300941012     C                   ELSE
091400970129      * Ristampo dal file FNSPA00F
091500941012     C                   EXSR      RISTAM
091600941012     C                   ENDIF
091700000615     C*
091800000615     C* SE DEVO ANCHE ELABORARE, LO FACCIO ORA
091900000615     C     SAVRIS        IFEQ      'S'
092000000615     C                   SETOFF                                       36
092100000615     C                   MOVEL     'R'           DS1RIS
092200000615     C                   MOVEA     SVLIN         LIN
092300000615     C                   EXSR      RISTAM
092400000615     C                   ENDIF
092500940103      *
092600930924     C**CALCOLI % E STAMPA DEI TOTALI GENERALI
092700941012      * SOLO SE STAMPATA ANCHE FILIALE DI 1° LIVELLO
092800000308      * OPPURE SE SONO IN RISTAMPA E HO STAMPATO LA RIGA DEI TRANSITI
092900000308     C     *IN08         IFEQ      *ON
093000000308     C     DS1RIS        OREQ      'R'
093100000308     C     TOTXKA        ANDGT     0
093200000308     C                   EXSR      TOTGEN
093300000308     C                   ENDIF
093400940405      *
093500941012      * Stampo la dicitura finale "N.B."
093600941012     C  N36              EXCEPT    FINSTA
093700931222      *
093800931222      *CHIUSURA FILE DI STAMPA
093900941012     C  N36              CLOSE     PRTF198
094000940223      *
094100941012      * Aggiorno la data ultima elaborazione
094200950210     C     DS1RIS        IFNE      'R'
094300021016
094400021016     c                   Eval      kTbeCod = 'SDF'
094500021016     c                   Movel(p)  Simfel        kTbeKe1
094600021016     c     Ktbe          Chain     Tntbe01l
094700021016     c                   If        %Found(Tntbe01l)
094800021016     c                   Movel     TbeUni        dSdf
094900021016     c                   If        §SdfParP = *Zeros or §SdfParP > Ds1Ada
095000021016     c                   Z-Add     Ds1Ada        §SdfParP
095100021016     c                   EndIf
095200021016     c                   If        §SdfParU < Ds1Ada
095300021016     c                   Z-Add     Ds1Ada        §SdfParU
095400021016     c                   EndIf
095500021016     c                   Movel     dSdf          TbeUni
095600021016     c                   Update    Tntbe000
095700021016     c                   EndIf
095800090415     C                   ENDIF
095900090415     c
096000090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
096100090415     c*  in FISTA
096200090415     c                   if        savris='S'
096300090415     c     ksta          delete    fista01l
096400941012     C                   ENDIF
096500940223      *
096600940509     C     ENDLEG        ENDSR
096700930924     C*****************************************************************
096800930924     C* SUBROUTINE ELALNP: ELABORAZIONE FILIALI (LINEE DI PARTENZA)
096900930924     C*****************************************************************
097000930924     C     ELALNP        BEGSR
097100950111     C*
097200931008     C                   Z-ADD     0             FLDTXT
097300931013     C*ELABORO COLLI
097400021023     C     KBLT30        SETLL     FNBLT38L
097500930924     C*
097600021023     C     KBLT30        READE     FNBLT38L                               31
097700930924     C* CICLO LETTURA COLLI
097800970130    1C     *IN31         DOWEQ     *OFF
097900931013     C* VEDERE I DATI  BOLLA SOLO AL CAMBIO BOLLA
098000970130    2C     DSTXT         IFNE      FLDTXT
098100950111     C                   Z-ADD     DSTXT         FLDTXT
098200950111     C     KBLP01        CHAIN     FNBLP01L                           32
098300931130      *
098400930927     C*SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
098500970130    3C     *IN32         IFEQ      *OFF
098600950111     C                   Z-ADD     1             WA                3 0
098700950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
098800950111     C   32              MOVEL     D3A(WA)       DS3A
098900950111     C  N32              CLEAR                   DS3A
099000931130      *
099100970505     C* REPERIMENTO DATI BOLLA
099200931007     C                   EXSR      DTBOL
099300931007     C* REPERIMENTO DATI BOLLA, PER CALCOLO.
099400930927     C                   EXSR      DATBOL
099500931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN UN RECUPERO
099600970130   X3C                   ELSE
099700930924     C                   MOVEL     'R'           §3ARBL
099800970130    3C                   ENDIF
099900970130    2C                   ENDIF
100000930924     C*
100100931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
100200970130    2C     §3ARBL        IFNE      'R'
100300930924     C*ESECUZIONE SUBROUTINE DEI CALCOLI
100400930924     C                   EXSR      CALC
100500970130    2C                   ENDIF
100600930924     C*
100700021023     C     KBLT30        READE     FNBLT38L                               31
100800970130    1C                   ENDDO
100900070523     c
101000070523     c* Se data elaborazione <= data calcolo affidabilità partenza
101100070523     c*  con data ritiro, calcolo a modo vecchio
101200070523     c                   if        ds1ada<§vponpi
101300930927     C***
101400931013     C*ELABORO SPEDIZIONI
101500970129     C**
101600970129     C** I° CICLO --> DUC = 0 DBR <= DATA ELABORAZIONE
101700970204     C*               ESCLUDENDO QUELLE CON DATA BORDERO' = 0
101800970129     C**
101900970129     C                   MOVE      *ZEROS        KDUC
102000970204     C                   MOVE      *ZEROS        KDBR
102100970204     C     KBLP6A        SETGT     FNBLP06L
102200970129     C     KBLP6         READE     FNBLP06L                               31
102300970130    1C     *IN31         DOWEQ     *OFF
102400970203     C     BLPDBR        ANDLE     DS1ADA
102500931130      * TEST SE E' RECUPERO
102600950111     C                   Z-ADD     1             WA
102700950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
102800950111     C   32              MOVEL     D3A(WA)       DS3A
102900950111     C  N32              CLEAR                   DS3A
103000930927     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
103100970203    2C     §3ARBL        IFNE      'R'
103200970304     C     BLPNFV        ANDNE     99999
103300970304     C     BLPNFV        ANDNE     99998
103400990907     C     BLPCBO        ANDNE     '£ '
103500070525     c     blppdr        andgt     0
103600931007     C                   EXSR      CSPE
103700970203    2C                   ENDIF
103800970203     C     KBLP6         READE     FNBLP06L                               31
103900970130    1C                   ENDDO
104000970129     C**
104100970206     C** II° CICLO --> DUC = DATA ELABORAZIONE
104200970129     C**
104300970218     C                   Z-ADD     DS1ADA        KDUC
104400970206     C     KBLP6         SETLL     FNBLP06L
104500970206     C     KBLP6         READE     FNBLP06L                               31
104600970130    1C     *IN31         DOWEQ     *OFF
104700970129      * TEST SE E' RECUPERO
104800970129     C                   Z-ADD     1             WA
104900970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
105000970129     C   32              MOVEL     D3A(WA)       DS3A
105100970129     C  N32              CLEAR                   DS3A
105200970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
105300970130    2C     §3ARBL        IFNE      'R'
105400970129     C                   EXSR      CSPE
105500970130    2C                   ENDIF
105600970206     C     KBLP6         READE     FNBLP06L                               31
105700970130    1C                   ENDDO
105800970129     C**
105900970129     C** III° CICLO --> DUC > DATA ELABORAZIONE; DBR <= DATA ELABORAZ.
106000970129     C**
106100970129     C                   Z-ADD     DS1ADA        BLPDUC
106200970129     C     KBLP6B        SETGT     FNBLP06L
106300970129     C     COMFIL        READE     FNBLP06L                               31
106400970130    1C     *IN31         DOWEQ     *OFF
106500970204     C*
106600970130    2C     BLPDBR        IFGT      DS1ADA
106700970129     C     KBLP6B        SETGT     FNBLP06L
106800970129     C                   GOTO      LEGBLP
106900970204     C                   ELSE
107000970204     C* ESCLUDO SE DBR = 0
107100970204     C     BLPDBR        IFEQ      *ZEROS
107200970204     C     KBLP6C        SETGT     FNBLP06L
107300970204     C                   GOTO      LEGBLP
107400970204     C                   END
107500970130    2C                   END
107600970129      * TEST SE E' RECUPERO
107700970129     C                   Z-ADD     1             WA
107800970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
107900970129     C   32              MOVEL     D3A(WA)       DS3A
108000970129     C  N32              CLEAR                   DS3A
108100970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
108200970130    2C     §3ARBL        IFNE      'R'
108300070525     C     BLPNFV        ANDNE     99999
108400070525     C     BLPNFV        ANDNE     99998
108500070525     C     BLPCBO        ANDNE     '£ '
108600070525     c     blppdr        andgt     0
108700970129     C                   EXSR      CSPE
108800970130    2C                   ENDIF
108900970129     C     LEGBLP        TAG
109000970203     C     COMFIL        READE     FNBLP06L                               31
109100970130    1C                   ENDDO
109200070523     c
109300070523     c                   else
109400070523     c                   exsr      CALSPEDNEW
109500070523     c                   endif
109600930924     C*
109700930924     C                   ENDSR
109800070523     C*****************************************************************
109900070523     C* CALCOLO LE SPEDIZIONI A MODO NUOVO CONSIDERANDO LA DATA
110000070523     C*   RITIRO MERCE
110100070523     C*****************************************************************
110200070523     C     CALSPEDNEW    BEGSR
110300070523     C***
110400070523     C*ELABORO SPEDIZIONI
110500070523     C**
110600070523     C** I° CICLO --> DUC = 0 Drt <= DATA ELABORAZIONE
110700070523     C**
110800070523     C                   MOVE      *ZEROS        KDUC
110900070523     C                   MOVE      *ZEROS        KDrt
111000070523     C     KBLP5A        SETGT     FNBLP05L
111100070523     C     KBLP5         READE     FNBLP05L                               31
111200070523    1C     *IN31         DOWEQ     *OFF
111300070523     C     BLPDrt        ANDLE     DS1ADA
111400070523      * TEST SE E' RECUPERO
111500070523     C                   Z-ADD     1             WA
111600070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
111700070523     C   32              MOVEL     D3A(WA)       DS3A
111800070523     C  N32              CLEAR                   DS3A
111900070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
112000070523    2C     §3ARBL        IFNE      'R'
112100070523     C     BLPNFV        ANDNE     99999
112200070523     C     BLPNFV        ANDNE     99998
112300070523     C     BLPCBO        ANDNE     '£ '
112400070525     C     BLPpdr        ANDgt     0
112500070523     C                   EXSR      CSPE
112600070523    2C                   ENDIF
112700070523     C     KBLP5         READE     FNBLP05L                               31
112800070523    1C                   ENDDO
112900070523     C**
113000070523     C** II° CICLO --> DUC = DATA ELABORAZIONE
113100070523     C**
113200070523     C                   Z-ADD     DS1ADA        KDUC
113300070523     C     KBLP5         SETLL     FNBLP05L
113400070523     C     KBLP5         READE     FNBLP05L                               31
113500070523    1C     *IN31         DOWEQ     *OFF
113600070523      * TEST SE E' RECUPERO
113700070523     C                   Z-ADD     1             WA
113800070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
113900070523     C   32              MOVEL     D3A(WA)       DS3A
114000070523     C  N32              CLEAR                   DS3A
114100070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
114200070523    2C     §3ARBL        IFNE      'R'
114300070525     C     BLPNFV        ANDNE     99999
114400070525     C     BLPNFV        ANDNE     99998
114500070525     C     BLPCBO        ANDNE     '£ '
114600070523     C                   EXSR      CSPE
114700070523    2C                   ENDIF
114800070523     C     KBLP5         READE     FNBLP05L                               31
114900070523    1C                   ENDDO
115000070523     C**
115100070523     C** III° CICLO --> DUC > DATA ELABORAZIONE; Drt <= DATA ELABORAZ.
115200070523     C**
115300070523     C                   Z-ADD     DS1ADA        BLPDUC
115400070523     C     KBLP5B        SETGT     FNBLP05L
115500070523     C     COMFIL        READE     FNBLP05L                               31
115600070523    1C     *IN31         DOWEQ     *OFF
115700070523     C*
115800070523    2C     BLPDrt        IFGT      DS1ADA
115900070523     C     KBLP5B        SETGT     FNBLP05L
116000070523     C                   GOTO      LEGBLP5
116100070523   x2C                   ELSE
116200070523      * TEST SE E' RECUPERO
116300070523     C                   Z-ADD     1             WA
116400070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
116500070523     C   32              MOVEL     D3A(WA)       DS3A
116600070523     C  N32              CLEAR                   DS3A
116700070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
116800070523    3C     §3ARBL        IFNE      'R'
116900070525     C     BLPNFV        ANDNE     99999
117000070525     C     BLPNFV        ANDNE     99998
117100070525     C     BLPCBO        ANDNE     '£ '
117200070525     C     BLPpdr        ANDgt     0
117300070523     C                   EXSR      CSPE
117400070523    3C                   ENDIF
117500070523    2C                   ENDIF
117600070523     C     LEGBLP5       TAG
117700070523     C     COMFIL        READE     FNBLP05L                               31
117800070523    1C                   ENDDO
117900070523     C                   ENDSR
118000000831     C*****************************************************************
118100000831     C* ELABORO LE ANOMALIE 105 PER LA LINEA INTERESSATA
118200000831     C*****************************************************************
118300000831     C     ELA105        BEGSR
118400070523     C                   Z-ADD     105           WCAA
118500000831     C     KANM          SETLL     FNANM000
118600000831     C     KANM          READE     FNANM000                               32
118700000831     C*
118800000831     C     *IN32         DOWEQ     *OFF
118900000831     C* SE ANOMAIA CHIUSA VUOL DIRE CHE ADESSOHO LA BOLLA
119000000831     C     ANMDCH        IFEQ      0
119100040601     c     anmfle        andeq     simfel
119200000831     C* SE NON E' UGUALE ALLA LINEA DI PARTENZA, VEDO SE CONFERMABILE
119300000831     C*  DA ME
119400000831     C     ANMFLS        IFNE      COMFIL
119500000831     C* SE E' IN T06 NON MEMORIZZO, ALTRIMENTI MEMORIZZO PER SIMFEL
119600000831     C     ANMFLS        LOOKUP    T06                                    32
119700000831     C     *IN32         IFEQ      *OFF
119800000831     C     COMFIL        ANDEQ     SIMFEL
119900000831     C* NON SAPENDO IL TIPO SERVIZIO, SOMMO NELLA DISTRIBUZIONE
120000000831     C                   ADD       1             SKA(6)
120100000831     C                   ENDIF
120200000831     C*
120300000831     C                   ELSE
120400000831     C                   ADD       1             SKA(6)
120500000831     C                   ENDIF
120600000831     C                   ENDIF
120700000831     C*
120800000831     C     KANM          READE     FNANM000                               32
120900000831     C                   ENDDO
121000000831     C*
121100000831     C                   ENDSR
121200930924     C*****************************************************************
121300931007     C* SUBROUTINE DTBOL: DIFFERENZIA TRA FILIALE E C/SERVIZI
121400931013     C* ESEGUITA SOLO PER BOLLE NORMALI.
121500930927     C*****************************************************************
121600931007     C     DTBOL         BEGSR
121700950111     C*
121800000831     C* VERIFICO SE LNP POSTE
121900000831     C     BLPLNP        IFNE      SAVLNP
122000000831     C     BLPLNP        CHAIN     AZORG01L                           32
122100000831     C  N32              MOVEL     ORGDE3        OG143
122200000831     C   32              CLEAR                   OG143
122300020813     ***C                   MOVEL     §OGPT         LNPPT             1
122400020813     c                   if        §OGntw = 'PPT'
122500020813     c                   movel     'S'           LNPPT             1
122600020813     c                   else
122700020813     c                   clear                   LNPPT
122800020813     c                   endif
122900000831     C                   MOVEL     BLPLNP        SAVLNP
123000000831     C                   ENDIF
123100000831     C*
123200970129     C* MEMORIZZO SE E' DISK B O ALTRI
123300970129     C     BLPSOP        IFEQ      '§'
123400970129     C     BLPNRS        ANDGT     *ZEROS
123500970129     C     BLPSOP        OREQ      '%'
123600970129     C     BLPNRS        ANDGT     *ZEROS
123700970129     C                   SETON                                        05
123800970129     C                   ELSE
123900970129     C                   SETOFF                                       05
124000970129     C                   END
124100970129     C*
124200930927     C*TEST SE E' C/SERVIZI O FILIALE
124300931007     C     §3ARBL        IFEQ      'C'
124400930927     C                   SETON                                        04
124500930927     C                   ELSE
124600930927     C                   SETOFF                                       04
124700930927     C                   ENDIF
124800930927     C*
124900930927     C* SE TIPO SPEDIZIONE E' ESPRESSO
125000930927     C     BLPTSP        IFEQ      'E'
125100930927     C* SE (FILIALE) C/SERVIZI
125200930927     C   04              Z-ADD     2             X
125300930927     C* SE FILIALE
125400930927     C  N04              Z-ADD     5             X
125500950111     C                   ELSE
125600000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
125700950111     C     BLPTSP        IFEQ      'D'
125800000619     C     BLPTSP        OREQ      'P'
125900930927     C* SE (FILIALE) C/SERVIZI
126000930927     C   04              Z-ADD     3             X
126100930927     C* SE FILIALE
126200930927     C  N04              Z-ADD     6             X
126300950111     C                   ELSE
126400950111     C*
126500950111     C* SE (FILIALE) C/SERVIZI
126600950111     C   04              Z-ADD     1             X                 1 0
126700950111     C* SE FILIALE
126800950111     C  N04              Z-ADD     4             X
126900950111     C*
127000930927     C                   ENDIF
127100950111     C                   ENDIF
127200931007     C*
127300940411      * Preparo l'indice per c/servizio di filiale (dal 7 al 9), per le schiere
127400940411     C   04X             ADD       6             Y                 1 0
127500931007     C*
127600931007     C                   ENDSR
127700931007     C************************************************************
127800931007     C* SUBROUTINE DATBOL: REPERIMENTO DATI BOLLA PER CALCOLI.
127900931007     C*****************************************************************
128000931007     C     DATBOL        BEGSR
128100021213     ***C*
128200021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
128300021213     ***C     BLPVLF        DIV(H)    BLPNCL        VOLUM            12 4
128400021213     ***C*
128500021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
128600021213     ***C     BLPPKF        DIV(H)    BLPNCL        PESCOL            8 1
128700021220      *
128800021220      * Verifico i dati misurati con la macchina smistacolli
128900021220      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
129000021220      * e calcolo il VOLUME unitario medio per collo da usare se non
129100021220      * tutti i colli sono stati volumati dalla smistacolli
129200021220      *
129300030213      **** Verifico se sono stati volumati tutti i colli
129400030213     c***                clear                   Wsivol
129500021220     c                   clear                   Wvolu
129600050111     c                   if        BLPncr = BLPncl
129700021220     c                   z-add     BLPvlc        Wvolu
129800030213     c***                eval      Wsivol =  'S'
129900021220   x1c                   else
130000021220if  2c                   if        BLPvlc >= BLPvlf
130100021220     c                   z-add     BLPvlc        Wvolu
130200021220   x2c                   else
130300021220     c                   z-add     BLPvlf        Wvolu
130400021220e   2c                   endif
130500021220e   1c                   endif
130600021220      *
130700021220      * Calcolo il volume medio
130800021220     c     Wvolu         div(H)    BLPncl        wVOLUM
130900021220      *
131000021220      * Verifico i dati misurati con la macchina smistacolli
131100021220      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
131200021220      * e calcolo il PESO   unitario medio per collo da usare se non
131300021220      * tutti i colli sono stati pesati   dalla smistacolli
131400021220      *
131500030213      **** Verifico se sono stati pesati   tutti i colli
131600030213     c***                clear                   Wsipes
131700021220     c                   clear                   Wpeso
131800050111     c                   if        BLPncp = BLPncl
131900021220     c                   z-add     BLPpkc        Wpeso
132000030213     c***                eval      Wsipes =  'S'
132100021220   c1c                   else
132200021220if  2c                   if        BLPpkc >= BLPpkf
132300021220     c                   z-add     BLPpkc        Wpeso
132400021220   x2c                   else
132500021220     c                   z-add     BLPpkf        Wpeso
132600021220e   2c                   endif
132700021220e   1c                   endif
132800021220      *
132900021220      * Calcolo il peso medio
133000021220     c     Wpeso         div(H)    BLPncl        wPESCOL
133100940328     C*
133200930927     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
133300021213     ***C     BLPPKF        IFLE      3500
133400021213     C     Wpeso         IFLE      3500
133500930927     C                   SETON                                        06
133600930927     C                   ELSE
133700930927     C                   SETOFF                                       06
133800930927     C                   ENDIF
133900930927     C*
134000931006     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
134100021213     ***C                   Z-ADD     BLPPKF        PESTAX            8 0
134200021213     C                   z-add     Wpeso         PESTAX
134300930927     C*
134400930927     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
134500021213     ***C     BLPVLF        MULT      §QTKPM        COMPKT
134600021213     C     Wvolu         MULT      §QTKPM        COMPKT
134700930927     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
134800930927     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
134900930927     C     COMPKT        IFGT      PESTAX
135000930927     C                   Z-ADD     COMPKT        PESTAX
135100930927     C                   END
135200930930     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
135300930927     C                   Z-ADD     0             COMPKT            8 0
135400930927     C     PESTAX        DIV       §3IARR        COMPKT
135500930927     C                   MVR                     RESTO             3 0
135600930927     C     RESTO         IFGT      0
135700930927     C     COMPKT        MULT      §3IARR        PESTAX
135800930927     C                   ADD       §3IARR        PESTAX
135900930930     C                   ENDIF
136000021213     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
136100021213     C     PESTAX        DIV(H)    BLPNCL        PESTAS
136200930927     C*
136300930927     C                   ENDSR
136400930927     C****************************************************************
136500930929     C* SUBROUTINE CALC:
136600930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
136700930927     C*****************************************************************
136800930927     C     CALC          BEGSR
136900930927     C*
137000930927     C******COLLI
137100930927     C                   ADD       1             SKA(X)
137200941012     C   04              ADD       1             SKA(Y)
137300930927     C*
137400930927     C******COLLI NON SPUNTATI IN PARTENZA: 88.
137500930927     C     BLTNPP        IFEQ      88
137600950328     C     BLTNPP        OREQ      89
137700970430     C     BLTNPP        OREQ      98
137800970430     C     BLTNPP        OREQ      96
137900000831     C* SOLO PER LE BOLLE POSTE, CONTO ANCHE I COLLI CON NPS=90
138000000831     C     LNPPT         OREQ      'S'
138100000831     C     BLTNPP        ANDEQ     90
138200970129     C* DISK B
138300970129     C     *IN05         IFEQ      *ON
138400970129     C*SE (FILIALE) C/SERVIZI
138500970129     C   04              ADD       1             PB88CS            7 0
138600970129     C   04              ADD       1             PB88              7 0
138700970129     C*SE FILIALE
138800970129     C  N04              ADD       1             PB88F             7 0
138900970129     C                   ELSE
139000970129     C* NO DISK B
139100970129     C*
139200930927     C*SE (FILIALE) C/SERVIZI
139300930927     C   04              ADD       1             PI88CS            7 0
139400941012     C   04              ADD       1             PS88              7 0
139500930927     C*SE FILIALE
139600930927     C  N04              ADD       1             PI88              7 0
139700970129     C                   END
139800930927     C                   ENDIF
139900950622     C******COLLI PISTOLA 87 (EX 88)
140000950622     C     BLTNPP        IFEQ      87
140100970129     C* DISK B
140200970129     C     *IN05         IFEQ      *ON
140300970129     C*SE (FILIALE) C/SERVIZI
140400970129     C   04              ADD       1             PB87CS            7 0
140500970129     C   04              ADD       1             PB87              7 0
140600970129     C*SE FILIALE
140700970129     C  N04              ADD       1             PB87F             7 0
140800970129     C                   ELSE
140900970129     C* NO DISK B
141000970129     C*
141100950622     C*SE (FILIALE) C/SERVIZI
141200950622     C   04              ADD       1             PI87CS            7 0
141300950622     C   04              ADD       1             PS87              7 0
141400950622     C*SE FILIALE
141500950622     C  N04              ADD       1             PI87              7 0
141600970129     C                   END
141700950622     C                   ENDIF
141800930927     C*
141900930927     C******PESO
142000930927     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
142100030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
142200030213      * Peso medio
142300030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
142400030213if  1c***                if        Wsipes = 'S'
142500030213     c***                z-add(H)  BLTpuc        PESCOL
142600030213   x1c***                else
142700021220     c                   z-add     Wpescol       PESCOL
142800030213e   1c***                endif
142900930927     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
143000930927     C   06              ADD       PESCOL        SKD(X)
143100941012     C   06
143200941012     CAN 04              ADD       PESCOL        SKD(Y)
143300970129     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
143400970129     C  N06              ADD       PESCOL        SKE(X)
143500970129     C  N06
143600970129     CAN 04              ADD       PESCOL        SKE(Y)
143700930927     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
143800930927     C                   ADD       PESCOL        SKF(X)
143900941012     C   04              ADD       PESCOL        SKF(Y)
144000930927     C*
144100930927     C******PESO TASSABILE
144200931007     C  N04
144300931007     CAN 06              ADD       PESTAS        PETS              9 1
144400931130      *
144500931130      ***** VOLUME
144600021220     *** * Se nel collo c'e' il volume utilizzo quello, altrimenti quello
144700021220     *** *  generico
144800021213     ***C     BLTVUC        IFNE      *ZERO
144900021213     ***C                   Z-ADD(H)  BLTVUC        WVOLUM
145000021213     ***C                   ELSE
145100021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
145200021213     ***C                   ENDIF
145300021220     ***C                   ADD       WVOLUM        SKI(X)
145400021220     ***C   04              ADD       WVOLUM        SKI(Y)
145500021220     *** *
145600021220      * VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
145700030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
145800030213      * Volume medio
145900030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
146000030213if  1c***                if        Wsivol = 'S'
146100030213     c***                z-add     BLTvuc        Volum
146200030213   x1c***                else
146300021220     c                   z-add     Wvolum        Volum
146400030213e   1c***                endif
146500021220     c                   add       VOLUM         SKI(X)
146600021220     c   04              add       VOLUM         SKI(Y)
146700931130      *
146800930927     C                   ENDSR
146900930927     C****************************************************************
147000931007     C* SUBROUTINE CSPE:  CALCOLO SPEDIZIONI
147100930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
147200930927     C*****************************************************************
147300931007     C     CSPE          BEGSR
147400930927     C*
147500931006     C*TEST SE E' C/SERVIZI O FILIALE
147600970228    1C     §3ARBL        IFEQ      'C'
147700931006     C*SE E' C/SERVIZI
147800931006     C                   SETON                                        04
147900970228   x1C                   ELSE
148000931006     C*SE E' FILIALE
148100931006     C                   SETOFF                                       04
148200970228    1C                   ENDIF
148300931006     C*
148400931006     C*FISSO GLI INDICI DI SCHIERA....
148500931006     C*
148600931006     C* SE TIPO SPEDIZIONE E' ESPRESSO
148700970228    1C     BLPTSP        IFEQ      'E'
148800931006     C* SE (FILIALE) C/SERVIZI
148900931006     C   04              Z-ADD     2             X
149000931006     C* SE FILIALE
149100931006     C  N04              Z-ADD     5             X
149200970228   X1C                   ELSE
149300000619     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE O POSTE
149400970228    2C     BLPTSP        IFEQ      'D'
149500000619    2C     BLPTSP        OREQ      'P'
149600931006     C* SE (FILIALE) C/SERVIZI
149700931006     C   04              Z-ADD     3             X
149800931006     C* SE FILIALE
149900931006     C  N04              Z-ADD     6             X
150000970228   x2C                   ELSE
150100950111     C*
150200950111     C* SE (FILIALE) C/SERVIZI
150300950111     C   04              Z-ADD     1             X                 1 0
150400950111     C* SE FILIALE
150500950111     C  N04              Z-ADD     4             X
150600950111     C*
150700970228    2C                   ENDIF
150800970228    1C                   ENDIF
150900940411      *
151000941012      * Se e' c/servizio aggiungo anche nel totale filiale c/serv.
151100941012     C   04X             ADD       6             Y
151200970228     C*
151300970206     C* Richiamo pgm per calcolo affidabilità
151400970303     C     BLPDCE        IFEQ      *ZEROS
151500970303     C     BLPDUC        ORNE      DS1ADA
151600970206     C                   MOVEL     ' '           D98TLA
151700970206     C                   MOVEL     'P'           D98TBO
151800970206     C                   MOVEL     BLPAAS        D98AAS
151900970206     C                   MOVEL     BLPLNP        D98LNP
152000970206     C                   MOVEL     BLPNRS        D98NRS
152100970206     C                   MOVEL     BLPNSP        D98NSP
152200970310     C                   Z-ADD     DS1ADA        D98DRF
152300970310     C                   Z-ADD     2359          D98ORF
152400970925     C                   CALL      'TNSD99R'
152500970925     C                   PARM                    DS99
152600970303     C                   ELSE
152700970303     C                   Z-ADD     BLPNPI        D98NPI
152800970303     C                   END
152900970206     C*
153000930927     C******SPEDIZIONI
153100970206    1C     BLPDUC        IFEQ      DS1ADA
153200970206     C*  PARTITO
153300930927     C                   ADD       1             SKL(X)
153400941012     C   04              ADD       1             SKL(Y)
153500970206     C*  PARTITO FUORI AFFIDABILITA'
153600970228     C* (SKO LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
153700970206    2C     D98NPI        IFGT      0
153800970206     C                   ADD       1             SKO(X)
153900970206     C   04              ADD       1             SKO(Y)
154000970206     C                   ADD       1             SKN(X)
154100970206     C   04              ADD       1             SKN(Y)
154200970206    2C                   END
154300970206   X1C                   ELSE
154400970206     C* A TERRA FUORI AFFIDABILITA'
154500970228     C* (SKP LO CONTO SOLO PER MEMORIZZARLO NEL FILE FNSPA)
154600970206    2C     D98NPI        IFGE      0
154700970206     C                   ADD       1             SKP(X)
154800970206     C   04              ADD       1             SKP(Y)
154900970206     C                   ADD       1             SKN(X)
155000970206     C   04              ADD       1             SKN(Y)
155100970206    2C                   END
155200970206    1C                   END
155300970129     C*  DA PARTIRE
155400970206     C                   MOVE      *BLANKS       COMDAP            1
155500970228     C* La distribuzione a terra del giorno non spareggiata non viene
155600970228     C* conteggiata nel "Da partire"
155700970303     C     BLPDUC        IFEQ      *ZEROS
155800970228     C     BLPDPC        ANDEQ     0
155900970206     C     D98NPI        ANDLT     0
156000970228     C*
156100970303     C     BLPDUC        OREQ      *ZEROS
156200970228     C     BLPDPC        ANDGT     DS1ADA
156300970228     C     D98NPI        ANDLT     0
156400970228     C*
156500970303     C     BLPDUC        ORGT      DS1ADA
156600970228     C     BLPDPC        ANDGT     DS1ADA
156700970228     C     D98NPI        ANDLT     0
156800970228     C*
156900970206     C                   MOVE      'N'           COMDAP
157000970206     C                   END
157100970206     C*
157200970206     C     COMDAP        IFEQ      *BLANKS
157300970129     C                   ADD       1             SKM(X)
157400970129     C   04              ADD       1             SKM(Y)
157500970206     C                   END
157600970228     C*
157700930927     C*
157800930927     C                   ENDSR
157900930927     C****************************************************************
158000930927     C* SUBROUTINE TOTLNP:
158100930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
158200930927     C*****************************************************************
158300930927     C     TOTLNP        BEGSR
158400950111     C*
158500930927     C***CALCOLO TOTALI COLLI
158600930927     C*TOTALE COLLI
158700930927     C     SKA(4)        ADD       SKA(5)        SKA456            7 0
158800930927     C                   ADD       SKA(6)        SKA456
158900930927     C**** **** **** **** **** **** **** **** ****
159000930927     C*
159100970129     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
159200930930     C     SKA456        IFNE      0
159300970129     C* DISK B
159400970203     C     PB88F         DIV(H)    SKA456        $XX               7 4
159500970129     C     $XX           MULT      100           PC88B             5 2
159600970129     C* ALTRI
159700970129     C     PI88          DIV(H)    SKA456        $XX
159800970129     C     $XX           MULT      100           PC88              5 2
159900970129     C* TOTALE
160000970129     C     PB88F         ADD       PI88          PI88T             7 0
160100970129     C     PI88T         DIV(H)    SKA456        $XX
160200970129     C     $XX           MULT      100           PC88T             5 2
160300930930     C                   ENDIF
160400931005     C*
160500970129     C                   CLEAR                   PER3T
160600970129     C     PB88F         IFNE      0
160700970129     C                   MOVE      '%'           PER3B             1
160800970129     C                   MOVE      '%'           PER3T             1
160900970129     C                   ELSE
161000970129     C                   MOVE      *BLANKS       PER3B
161100970129     C                   ENDIF
161200970129     C*
161300931005     C     PI88          IFNE      0
161400931005     C                   MOVE      '%'           PER3              1
161500970129     C                   MOVE      '%'           PER3T             1
161600931005     C                   ELSE
161700931005     C                   MOVE      *BLANKS       PER3
161800931005     C                   ENDIF
161900950622     C*
162000950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
162100950622     C     SKA456        IFNE      0
162200970129     C* DISK B
162300970129     C     PB87F         DIV(H)    SKA456        $XX
162400970129     C     $XX           MULT      100           PC87B             5 2
162500970129     C* ALTRI
162600950622     C     PI87          DIV(H)    SKA456        $XX
162700950622     C     $XX           MULT      100           PC87              5 2
162800970129     C* TOTALI
162900970129     C     PB87F         ADD       PI87          PI87T             7 0
163000970129     C     PI87T         DIV(H)    SKA456        $XX
163100970129     C     $XX           MULT      100           PC87T             5 2
163200950622     C                   ENDIF
163300950622     C*
163400970129     C                   CLEAR                   PER34T
163500950622     C     PI87          IFNE      0
163600950622     C                   MOVE      '%'           PER34             1
163700970129     C                   MOVE      '%'           PER34T            1
163800950622     C                   ELSE
163900950622     C                   MOVE      *BLANKS       PER34
164000950622     C                   ENDIF
164100970129     C     PB87F         IFNE      0
164200970129     C                   MOVE      '%'           PER34B            1
164300970129     C                   MOVE      '%'           PER34T            1
164400970129     C                   ELSE
164500970129     C                   MOVE      *BLANKS       PER34B
164600970129     C                   ENDIF
164700930927     C**** **** **** **** **** **** **** **** ****
164800930927     C***CALCOLO TOTALI PESO
164900930927     C*TOTALE PESO FINO A 35 QUINTALI
165000930927     C     SKD(4)        ADD       SKD(5)        SKD456            8 1
165100930927     C                   ADD       SKD(6)        SKD456
165200930927     C*TOTALE PESO OLTRE  35 QUINTALI
165300930927     C     SKE(4)        ADD       SKE(5)        SKE456            8 1
165400930927     C                   ADD       SKE(6)        SKE456
165500930927     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
165600930927     C     SKF(4)        ADD       SKF(5)        SKF456            8 1
165700930927     C                   ADD       SKF(6)        SKF456
165800930927     C**** **** **** **** **** **** **** **** ****
165900970129     C***CALCOLO TOTALE VOLUME
166000930927     C*TOTALE VOLUMI
166100941005     C     SKI(4)        ADD       SKI(5)        SKI456            9 4
166200930927     C                   ADD       SKI(6)        SKI456
166300930927     C**** **** **** **** **** **** **** **** ****
166400930927     C****CALCOLO % E TOTALI SPEDIZIONI
166500970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
166600970226     C* DA PARTIRE E FUORI AFFIDABILITA'
166700970226     C     WFES          IFEQ      'F'
166800970226     C     SKA(4)        ANDEQ     *ZEROS
166900970226     C     SKA(5)        ANDEQ     *ZEROS
167000970226     C     SKA(6)        ANDEQ     *ZEROS
167100970226     C                   CLEAR                   SKM(4)
167200970226     C                   CLEAR                   SKM(5)
167300970226     C                   CLEAR                   SKM(6)
167400970226     C                   CLEAR                   SKN(4)
167500970226     C                   CLEAR                   SKN(5)
167600970226     C                   CLEAR                   SKN(6)
167700970226     C                   CLEAR                   SKP(4)
167800970226     C                   CLEAR                   SKP(5)
167900970226     C                   CLEAR                   SKP(6)
168000970226     C                   END
168100930927     C***CALCOLO TOTALI SPEDIZIONI
168200970129     C*TOTALE SPEDIZIONI PARTITE
168300930927     C     SKL(4)        ADD       SKL(5)        SKL456            7 0
168400930927     C                   ADD       SKL(6)        SKL456
168500970305     C*TOTALE SPEDIZIONI DA PARTIRE
168600970129     C     SKM(4)        ADD       SKM(5)        SKM456            7 0
168700970129     C                   ADD       SKM(6)        SKM456
168800930927     C**** **** **** **** **** **** **** **** ****
168900930928     C****STAMPA SINGOLA FILIALE :DETTAGLIO E TOTALE.
169000970226     C*
169100931011     C     *INOF         IFEQ      *ON
169200941012     C  N36              EXCEPT    TESTA
169300020813     C                   Z-ADD     1             XD
169400931011     C                   ENDIF
169500931011     C*
169600941012     C  N36              EXCEPT    DETFIL
169700950210     C     DS1RIS        IFNE      'R'
169800941012     C                   EXSR      WRIFIL
169900941012     C                   ENDIF
170000930928     C**** **** **** **** **** **** **** **** ****
170100930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE, IN CAMPI AREA
170200000616     C** SOLO SE DEVO STAMPARE
170300000616     C     DS1RIS        IFNE      ' '
170400930929     C                   ADD       SKA456        AKA123
170500930929     C                   ADD       SKA(4)        AKA(1)
170600930929     C                   ADD       SKA(5)        AKA(2)
170700930929     C                   ADD       SKA(6)        AKA(3)
170800930929     C*
170900930929     C                   ADD       PI88          ARPI88            7 0
171000970129     C                   ADD       PB88F         ARPB88            7 0
171100970130     C                   ADD       PI88T         ARP88T            7 0
171200950622     C                   ADD       PI87          ARPI87            7 0
171300970130     C                   ADD       PB87F         ARPB87            7 0
171400970130     C                   ADD       PI87T         ARP87T            7 0
171500930929     C*
171600930929     C                   ADD       SKD456        AKD123
171700930929     C                   ADD       SKD(4)        AKD(1)
171800930929     C                   ADD       SKD(5)        AKD(2)
171900930929     C                   ADD       SKD(6)        AKD(3)
172000930929     C*
172100930929     C                   ADD       SKE456        AKE123
172200930929     C                   ADD       SKE(4)        AKE(1)
172300930929     C                   ADD       SKE(5)        AKE(2)
172400930929     C                   ADD       SKE(6)        AKE(3)
172500930929     C*
172600930929     C                   ADD       SKF456        AKF123
172700930929     C                   ADD       SKF(4)        AKF(1)
172800930929     C                   ADD       SKF(5)        AKF(2)
172900930929     C                   ADD       SKF(6)        AKF(3)
173000930929     C*
173100931007     C                   ADD       PETS          ARPETS            9 1
173200930929     C*
173300930929     C                   ADD       SKI456        AKI123
173400930929     C                   ADD       SKI(4)        AKI(1)
173500930929     C                   ADD       SKI(5)        AKI(2)
173600930929     C                   ADD       SKI(6)        AKI(3)
173700970129     C*
173800930929     C                   ADD       SKL456        AKL123
173900930929     C                   ADD       SKL(4)        AKL(1)
174000930929     C                   ADD       SKL(5)        AKL(2)
174100930929     C                   ADD       SKL(6)        AKL(3)
174200930929     C*
174300930929     C                   ADD       SKM456        AKM123
174400930929     C                   ADD       SKM(4)        AKM(1)
174500930929     C                   ADD       SKM(5)        AKM(2)
174600930929     C                   ADD       SKM(6)        AKM(3)
174700930929     C*
174800930929     C                   ADD       SKN(4)        AKN(1)
174900930929     C                   ADD       SKN(5)        AKN(2)
175000930929     C                   ADD       SKN(6)        AKN(3)
175100000616     C                   ENDIF
175200930929     C*
175300930929     C**** **** **** **** **** **** **** **** ****
175400930929     C****PULIZIA CAMPI FILIALE (DI TOTALE E DI DETTAGLIO)
175500930929     C*CAMPI DI TOTALE
175600930929     C                   Z-ADD     0             SKA456
175700930929     C                   Z-ADD     0             PI88
175800941012     C                   Z-ADD     0             PS88
175900930929     C                   Z-ADD     0             PC88
176000970129     C                   Z-ADD     0             PB88F
176100970129     C                   Z-ADD     0             PB88
176200970129     C                   Z-ADD     0             PC88B
176300970130     C                   Z-ADD     0             PI88T
176400970129     C                   Z-ADD     0             PC88T
176500950622     C                   Z-ADD     0             PI87
176600950622     C                   Z-ADD     0             PS87
176700950622     C                   Z-ADD     0             PC87
176800970129     C                   Z-ADD     0             PB87F
176900970129     C                   Z-ADD     0             PB87
177000970129     C                   Z-ADD     0             PC87B
177100970130     C                   Z-ADD     0             PI87T
177200970129     C                   Z-ADD     0             PC87T
177300930929     C                   Z-ADD     0             SKD456
177400930929     C                   Z-ADD     0             SKE456
177500930929     C                   Z-ADD     0             SKF456
177600930929     C                   Z-ADD     0             PETS
177700930929     C                   Z-ADD     0             SKI456
177800930929     C                   Z-ADD     0             SKL456
177900930929     C                   Z-ADD     0             SKM456
178000930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
178100930929     C*PER LA STAMPA).
178200931005     C                   MOVEA     *ZEROS        SKA(4)
178300930929     C*
178400931005     C                   MOVEA     *ZEROS        SKD(4)
178500930929     C*
178600931005     C                   MOVEA     *ZEROS        SKE(4)
178700930929     C*
178800931005     C                   MOVEA     *ZEROS        SKF(4)
178900930929     C*
179000931005     C                   MOVEA     *ZEROS        SKI(4)
179100930929     C*
179200931005     C                   MOVEA     *ZEROS        SKL(4)
179300930929     C*
179400931005     C                   MOVEA     *ZEROS        SKM(4)
179500930929     C*
179600931005     C                   MOVEA     *ZEROS        SKN(4)
179700970206     C                   MOVEA     *ZEROS        SKO(4)
179800970206     C                   MOVEA     *ZEROS        SKP(4)
179900930929     C*
180000930927     C                   ENDSR
180100930929     C****************************************************************
180200930929     C* SUBROUTINE TOTCS:
180300930929     C**CALCOLO %, CALCOLO TOTALI C/SERVIZI, STAMPA DETTAGLIO C/SERVIZI,
180400930929     C*****************************************************************
180500930929     C     TOTCS         BEGSR
180600950111     C*
180700930929     C***CALCOLO TOTALI COLLI
180800930929     C*TOTALE COLLI
180900930929     C     SKA(1)        ADD       SKA(2)        SKA123            7 0
181000930929     C                   ADD       SKA(3)        SKA123
181100930929     C**** **** **** **** **** **** **** **** ****
181200970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
181300930930     C     SKA123        IFNE      0
181400970130     C* DISK B
181500970130     C     PB88CS        DIV(H)    SKA123        $XX
181600970130     C     $XX           MULT      100           PC88CB            5 2
181700970130     C* ALTRI
181800931005     C     PI88CS        DIV(H)    SKA123        $XX
181900931005     C     $XX           MULT      100           PC88CS            5 2
182000970130     C* TOTALE
182100970130     C     PB88CS        ADD       PI88CS        P88CST            7 0
182200970130     C     P88CST        DIV(H)    SKA123        $XX
182300970130     C     $XX           MULT      100           PC88CT            5 2
182400930930     C                   ENDIF
182500931005     C*
182600970130     C                   CLEAR                   PER3T
182700970130     C     PB88CS        IFNE      0
182800970130     C                   MOVE      '%'           PER3B
182900970130     C                   MOVE      '%'           PER3T
183000931005     C                   ELSE
183100970130     C                   MOVE      *BLANKS       PER3B
183200931005     C                   ENDIF
183300970130     C     PI88CS        IFNE      0
183400970130     C                   MOVE      '%'           PER3
183500970130     C                   MOVE      '%'           PER3T
183600970130     C                   ELSE
183700970130     C                   MOVE      *BLANKS       PER3
183800970130     C                   ENDIF
183900950622     C****CALCOLO % COLLI NON SPUNTATI CON PISTOLA 87 (EX 88)
184000950622     C     SKA123        IFNE      0
184100970130     C* DISK B
184200970130     C     PB87CS        DIV(H)    SKA123        $XX
184300970130     C     $XX           MULT      100           PC87CB            5 2
184400970130     C* ALTRI
184500950622     C     PI87CS        DIV(H)    SKA123        $XX
184600950622     C     $XX           MULT      100           PC87CS            5 2
184700970130     C* TOTALI
184800970130     C     PB87CS        ADD       PI87CS        P87CST            7 0
184900970203     C     P87CST        DIV(H)    SKA123        $XX
185000970130     C     $XX           MULT      100           PC87CT            5 2
185100950622     C                   ENDIF
185200950622     C*
185300970130     C                   CLEAR                   PER34T
185400970130     C     PB87CS        IFNE      0
185500970130     C                   MOVE      '%'           PER34B
185600970130     C                   MOVE      '%'           PER34T
185700950622     C                   ELSE
185800970130     C                   MOVE      *BLANKS       PER34B
185900950622     C                   ENDIF
186000970130     C     PI87CS        IFNE      0
186100970130     C                   MOVE      '%'           PER34
186200970130     C                   MOVE      '%'           PER34T
186300970130     C                   ELSE
186400970130     C                   MOVE      *BLANKS       PER34
186500970130     C                   ENDIF
186600930929     C**** **** **** **** **** **** **** **** ****
186700930929     C***CALCOLO TOTALI PESO
186800930929     C*TOTALE PESO FINO A 35 QUINTALI
186900930929     C     SKD(1)        ADD       SKD(2)        SKD123            8 1
187000930929     C                   ADD       SKD(3)        SKD123
187100930929     C*TOTALE PESO OLTRE  35 QUINTALI
187200930929     C     SKE(1)        ADD       SKE(2)        SKE123            8 1
187300930929     C                   ADD       SKE(3)        SKE123
187400930929     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
187500930929     C     SKF(1)        ADD       SKF(2)        SKF123            8 1
187600930929     C                   ADD       SKF(3)        SKF123
187700930929     C**** **** **** **** **** **** **** **** ****
187800930929     C***CALCOLO TOTALI VOLUMI
187900930929     C*TOTALE VOLUMI
188000941005     C     SKI(1)        ADD       SKI(2)        SKI123            9 4
188100930929     C                   ADD       SKI(3)        SKI123
188200930929     C**** **** **** **** **** **** **** **** ****
188300930929     C****CALCOLO % E TOTALI SPEDIZIONI
188400970130     C***CALCOLO % SPEDIZIONI FUORI AFFIDABILITA'
188500970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
188600970226     C* DA PARTIRE E FUORI AFFIDABILITA'
188700970226     C     WFES          IFEQ      'F'
188800970226     C     SKA(1)        ANDEQ     *ZEROS
188900970226     C     SKA(2)        ANDEQ     *ZEROS
189000970226     C     SKA(3)        ANDEQ     *ZEROS
189100970226     C                   CLEAR                   SKM(1)
189200970226     C                   CLEAR                   SKM(2)
189300970226     C                   CLEAR                   SKM(3)
189400970226     C                   CLEAR                   SKN(1)
189500970226     C                   CLEAR                   SKN(2)
189600970226     C                   CLEAR                   SKN(3)
189700970226     C                   CLEAR                   SKP(1)
189800970226     C                   CLEAR                   SKP(2)
189900970226     C                   CLEAR                   SKP(3)
190000970226     C                   END
190100930929     C*
190200930929     C***CALCOLO TOTALI SPEDIZIONI
190300970130     C*TOTALE SPEDIZIONI PARTITE
190400930929     C     SKL(1)        ADD       SKL(2)        SKL123            7 0
190500930929     C                   ADD       SKL(3)        SKL123
190600970130     C*TOTALE SPEDIZIONI DA PARTIRE
190700930929     C     SKM(1)        ADD       SKM(2)        SKM123            7 0
190800930929     C                   ADD       SKM(3)        SKM123
190900930929     C**** **** **** **** **** **** **** **** ****
191000930929     C****STAMPA FILIALE C/SERVIZI: DETTAGLIO E TOTALE.
191100931011     C     *INOF         IFEQ      *ON
191200941012     C  N36              EXCEPT    TESTA
191300020813     C                   Z-ADD     0             XD
191400931011     C                   ENDIF
191500931011     C*
191600941012     C  N36              EXCEPT    DETCS
191700930929     C**** **** **** **** **** **** **** **** ****
191800930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE C/SERVIZI, IN CAMPI AREA
191900000616     C* SOLO SE STAMPO
192000000616     C     DS1RIS        IFNE      ' '
192100930930     C                   ADD       SKA123        AKA123            7 0
192200930929     C                   ADD       SKA(1)        AKA(1)
192300930929     C                   ADD       SKA(2)        AKA(2)
192400930929     C                   ADD       SKA(3)        AKA(3)
192500930929     C*
192600930929     C                   ADD       PI88CS        ARPI88            7 0
192700970130     C                   ADD       PB88CS        ARPB88            7 0
192800950622     C                   ADD       PI87CS        ARPI87            7 0
192900970130     C                   ADD       PB87CS        ARPB87            7 0
193000970130     C                   ADD       P88CST        ARP88T            7 0
193100970130     C                   ADD       P87CST        ARP87T            7 0
193200930929     C*
193300930930     C                   ADD       SKD123        AKD123            8 1
193400930929     C                   ADD       SKD(1)        AKD(1)
193500930929     C                   ADD       SKD(2)        AKD(2)
193600930929     C                   ADD       SKD(3)        AKD(3)
193700930929     C*
193800930930     C                   ADD       SKE123        AKE123            8 1
193900930929     C                   ADD       SKE(1)        AKE(1)
194000930929     C                   ADD       SKE(2)        AKE(2)
194100930929     C                   ADD       SKE(3)        AKE(3)
194200930929     C*
194300930930     C                   ADD       SKF123        AKF123            8 1
194400930929     C                   ADD       SKF(1)        AKF(1)
194500930929     C                   ADD       SKF(2)        AKF(2)
194600930929     C                   ADD       SKF(3)        AKF(3)
194700930929     C*
194800941005     C                   ADD       SKI123        AKI123            9 4
194900930929     C                   ADD       SKI(1)        AKI(1)
195000930929     C                   ADD       SKI(2)        AKI(2)
195100930929     C                   ADD       SKI(3)        AKI(3)
195200930929     C*
195300930930     C                   ADD       SKL123        AKL123            7 0
195400930930     C                   ADD       SKL(1)        AKL(1)
195500930929     C                   ADD       SKL(2)        AKL(2)
195600930929     C                   ADD       SKL(3)        AKL(3)
195700930929     C*
195800930930     C                   ADD       SKM123        AKM123            7 0
195900930929     C                   ADD       SKM(1)        AKM(1)
196000930929     C                   ADD       SKM(2)        AKM(2)
196100930929     C                   ADD       SKM(3)        AKM(3)
196200930929     C*
196300930929     C                   ADD       SKN(1)        AKN(1)
196400930929     C                   ADD       SKN(2)        AKN(2)
196500930929     C                   ADD       SKN(3)        AKN(3)
196600000616     C                   ENDIF
196700930929     C*
196800930929     C**** **** **** **** **** **** **** **** ****
196900930929     C****PULIZIA CAMPI FILIALE C/SERVIZI (DI TOTALE E DI DETTAGLIO)
197000930929     C*CAMPI DI TOTALE
197100930929     C                   Z-ADD     0             SKA123
197200930929     C                   Z-ADD     0             PI88CS
197300970130     C                   Z-ADD     0             PB88CS
197400930929     C                   Z-ADD     0             PC88CS
197500970130     C                   Z-ADD     0             PC88CB
197600970130     C                   Z-ADD     0             P88CST
197700970130     C                   Z-ADD     0             PC88CT
197800950622     C                   Z-ADD     0             PI87CS
197900970130     C                   Z-ADD     0             PB87CS
198000950622     C                   Z-ADD     0             PC87CS
198100970130     C                   Z-ADD     0             PC87CB
198200970130     C                   Z-ADD     0             P87CST
198300970130     C                   Z-ADD     0             PC87CT
198400930929     C                   Z-ADD     0             SKD123
198500930929     C                   Z-ADD     0             SKE123
198600930929     C                   Z-ADD     0             SKF123
198700930929     C                   Z-ADD     0             SKI123
198800930929     C                   Z-ADD     0             SKL123
198900930929     C                   Z-ADD     0             SKM123
199000930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
199100930929     C*PER LA STAMPA).
199200930929     C*
199300931005     C                   MOVEA     *ZEROS        SKA(1)
199400930929     C*
199500931005     C                   MOVEA     *ZEROS        SKD(1)
199600930929     C*
199700931005     C                   MOVEA     *ZEROS        SKE(1)
199800930929     C*
199900931005     C                   MOVEA     *ZEROS        SKF(1)
200000930929     C*
200100931005     C                   MOVEA     *ZEROS        SKI(1)
200200930929     C*
200300931005     C                   MOVEA     *ZEROS        SKL(1)
200400930929     C*
200500931005     C                   MOVEA     *ZEROS        SKM(1)
200600930929     C*
200700931005     C                   MOVEA     *ZEROS        SKN(1)
200800970206     C                   MOVEA     *ZEROS        SKO(1)
200900970206     C                   MOVEA     *ZEROS        SKP(1)
201000930929     C*
201100930929     C                   ENDSR
201200930929     C****************************************************************
201300930929     C* SUBROUTINE TOTARE:
201400930929     C**CALCOLO %, CALCOLO TOTALI AREA, STAMPA DETTAGLIO AREA,
201500930929     C*****************************************************************
201600930929     C     TOTARE        BEGSR
201700970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
201800930930     C     AKA123        IFNE      0
201900970130     C* DISK B
202000970130     C     ARPB88        DIV(H)    AKA123        $XX
202100970130     C     $XX           MULT      100           APC88B            5 2
202200970130     C* ALTRI
202300970130     C     ARPI88        DIV(H)    AKA123        $XX
202400970130     C     $XX           MULT      100           ARPC88            5 2
202500970130     C* TOTALE
202600970130     C     ARP88T        DIV(H)    AKA123        $XX
202700970130     C     $XX           MULT      100           APC88T            5 2
202800930930     C                   ENDIF
202900931005     C*
203000970130     C                   CLEAR                   PER3T
203100970130     C     ARPB88        IFNE      0
203200970130     C                   MOVE      '%'           PER3B
203300970130     C                   MOVE      '%'           PER3T
203400931005     C                   ELSE
203500970130     C                   MOVE      *BLANKS       PER3B
203600931005     C                   ENDIF
203700970130     C     ARPI88        IFNE      0
203800970130     C                   MOVE      '%'           PER3
203900970130     C                   MOVE      '%'           PER3T
204000970130     C                   ELSE
204100970130     C                   MOVE      *BLANKS       PER3
204200970130     C                   ENDIF
204300950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
204400950622     C     AKA123        IFNE      0
204500970130     C* DISK B
204600970130     C     ARPB87        DIV(H)    AKA123        $XX
204700970130     C     $XX           MULT      100           APC87B            5 2
204800970130     C* ALTRI
204900950622     C     ARPI87        DIV(H)    AKA123        $XX
205000950622     C     $XX           MULT      100           ARPC87            5 2
205100970130     C* TOTALE
205200970130     C     ARP87T        DIV(H)    AKA123        $XX
205300970130     C     $XX           MULT      100           APC87T            5 2
205400950622     C                   ENDIF
205500950622     C*
205600970130     C                   CLEAR                   PER34T
205700970130     C     ARPB87        IFNE      0
205800970130     C                   MOVE      '%'           PER34B
205900970130     C                   MOVE      '%'           PER34T
206000950622     C                   ELSE
206100970130     C                   MOVE      *BLANKS       PER34B
206200950622     C                   ENDIF
206300970130     C     ARPI87        IFNE      0
206400970130     C                   MOVE      '%'           PER34
206500970130     C                   MOVE      '%'           PER34T
206600970130     C                   ELSE
206700970130     C                   MOVE      *BLANKS       PER34
206800970130     C                   ENDIF
206900930929     C**** **** **** **** **** **** **** **** ****
207000930929     C****STAMPA AREA :DETTAGLIO E TOTALE.
207100020813     C     XD            IFLE      7
207200941012     C  N36              EXCEPT    DETARE
207300931011     C                   ELSE
207400941012     C  N36              EXCEPT    TESTA
207500941012     C  N36              EXCEPT    DETARE
207600931011     C                   ENDIF
207700930929     C**** **** **** **** **** **** **** **** ****
207800931007     C****SOMMA CAMPI TOTALI AREA, IN CAMPI TOT. GENERALE
207900000616     C* SOLO SE  STAMPO
208000000616     C     DS1RIS        IFNE      ' '
208100931006     C*
208200931008     C                   ADD       AKA123        TKA123            7 0
208300931006     C                   ADD       AKA(1)        TKA(1)
208400931006     C                   ADD       AKA(2)        TKA(2)
208500931006     C                   ADD       AKA(3)        TKA(3)
208600931006     C*
208700970130     C                   ADD       ARP88T        TGP88T            7 0
208800970130     C                   ADD       ARP87T        TGP87T            7 0
208900931006     C*
209000931008     C                   ADD       AKD123        TKD123            8 1
209100931006     C                   ADD       AKD(1)        TKD(1)
209200931006     C                   ADD       AKD(2)        TKD(2)
209300931006     C                   ADD       AKD(3)        TKD(3)
209400931006     C*
209500931008     C                   ADD       AKE123        TKE123            8 1
209600931006     C                   ADD       AKE(1)        TKE(1)
209700931006     C                   ADD       AKE(2)        TKE(2)
209800931006     C                   ADD       AKE(3)        TKE(3)
209900931006     C*
210000931008     C                   ADD       AKF123        TKF123            8 1
210100931006     C                   ADD       AKF(1)        TKF(1)
210200931006     C                   ADD       AKF(2)        TKF(2)
210300931006     C                   ADD       AKF(3)        TKF(3)
210400931006     C*
210500931007     C                   ADD       ARPETS        TGPETS            9 1
210600931006     C*
210700941005     C                   ADD       AKI123        TKI123            9 4
210800931006     C                   ADD       AKI(1)        TKI(1)
210900931006     C                   ADD       AKI(2)        TKI(2)
211000931006     C                   ADD       AKI(3)        TKI(3)
211100931006     C*
211200931008     C                   ADD       AKL123        TKL123            7 0
211300931006     C                   ADD       AKL(1)        TKL(1)
211400931006     C                   ADD       AKL(2)        TKL(2)
211500931006     C                   ADD       AKL(3)        TKL(3)
211600000616     C                   ENDIF
211700931006     C*
211800930930     C**** **** **** **** **** **** **** **** ****
211900930930     C****PULIZIA CAMPI AREA    (DI TOTALE E DI DETTAGLIO)
212000930929     C*CAMPI DI TOTALE
212100930930     C                   Z-ADD     0             AKA123
212200930930     C                   Z-ADD     0             ARPI88
212300970130     C                   Z-ADD     0             ARPB88
212400970130     C                   Z-ADD     0             APC88B
212500970130     C                   Z-ADD     0             ARPC88
212600970130     C                   Z-ADD     0             ARP88T
212700970130     C                   Z-ADD     0             APC88T
212800950622     C                   Z-ADD     0             ARPI87
212900970130     C                   Z-ADD     0             ARPB87
213000950622     C                   Z-ADD     0             ARPC87
213100970130     C                   Z-ADD     0             APC87B
213200970130     C                   Z-ADD     0             ARP87T
213300970130     C                   Z-ADD     0             APC87T
213400930930     C                   Z-ADD     0             AKD123
213500930930     C                   Z-ADD     0             AKE123
213600930930     C                   Z-ADD     0             AKF123
213700930930     C                   Z-ADD     0             ARPETS
213800930930     C                   Z-ADD     0             AKI123
213900930930     C                   Z-ADD     0             AKL123
214000930930     C                   Z-ADD     0             AKM123
214100930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
214200930929     C*PER LA STAMPA).
214300931005     C                   MOVEA     *ZEROS        AKA(1)
214400930929     C*
214500931005     C                   MOVEA     *ZEROS        AKD(1)
214600930929     C*
214700931005     C                   MOVEA     *ZEROS        AKE(1)
214800930929     C*
214900931005     C                   MOVEA     *ZEROS        AKF(1)
215000930929     C*
215100931005     C                   MOVEA     *ZEROS        AKI(1)
215200930929     C*
215300931005     C                   MOVEA     *ZEROS        AKL(1)
215400930929     C*
215500931005     C                   MOVEA     *ZEROS        AKM(1)
215600930929     C*
215700931005     C                   MOVEA     *ZEROS        AKN(1)
215800930929     C*
215900930929     C                   ENDSR
216000930929     C****************************************************************
216100931006     C* SUBROUTINE ELATRA: ELABORAZIONE TRANSITI
216200931006     C*****************************************************************
216300931006     C     ELATRA        BEGSR
216400950111     C*
216500931008     C                   Z-ADD     0             FLDTXT
216600950111     C*
216700950111     C* ?    C O L L I    T R A N S I T O    ?
216800950329     C*
216900931007     C*POSIZ.PER'DATA DA'(DATA USCITA DAL TRANSITO).
217000020823     C     KDTA          SETLL     FNBTT38L
217100020823     C     KDTA          READE     FNBTT38L                               31
217200931006     C* CICLO LETTURA COLLI
217300970130    1C     *IN31         DOWEQ     *OFF
217400931007     C*
217500931013     C* ENTRA SOLO AL CAMBIO BOLLA.
217600970130    2C     DSTXT1        IFNE      FLDTXT
217700950111     C                   Z-ADD     DSTXT1        FLDTXT
217800020823     C     KBTP11        CHAIN     FNBTP11L                           32
217900931130      *
218000931130      * SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
218100970130    3C     *IN32         IFEQ      *OFF
218200950111     C*
218300950111     C                   Z-ADD     1             WA
218400950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
218500950111     C   32              MOVEL     D3A(WA)       DS3A
218600950111     C  N32              CLEAR                   DS3A
218700931130      *
218800950111     C* REPERIM.DATI BOLLA, (CORR.,ESPR.,RAPID,NON DIFFERENZIANDO I C/SERV.)
218900931007     C* QUINDI SOLO PER BOLLE-TRANSITO.
219000931007     C                   EXSR      DTBOT
219100950111     C* REPERIMENTO DATI BOLLA, PER CALCOLO TRANSITI
219200950111     C                   EXSR      DATBOT
219300931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN RECUPERO PER FARLA SCARTARE
219400970130   X3C                   ELSE
219500931006     C                   MOVEL     'R'           §3ARBL
219600970130    3C                   ENDIF
219700970130    2C                   ENDIF
219800931006     C*
219900931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
220000970130    2C     §3ARBL        IFNE      'R'
220100931013     C* CALCOLI-TRANSITI
220200931007     C                   EXSR      CALCTR
220300970130    2C                   ENDIF
220400931006     C*
220500020823     C     KDTA          READE     FNBTT38L                               31
220600970130    1C                   ENDDO
220700970505     C* AGGIUNGO I COLLI IN DISGUIDO
220800970505     C                   ADD       WDIS88        TRPI88
220900000616     C                   CLEAR                   WDIS88
221000931006     C***
221100950111     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
221200950111     C* ?    S P E D I Z I O N I    T R A N S I T O    ?
221300931007     C*
221400950111     C* POSIZIONAMENTO PER DATA INIZIO PERIODO
221500020823     C     KDTA          SETLL     FNBTP15L
221600020823     C     KDTA          READE     FNBTP15L                               31
221700931013     C*
221800970130    1C     *IN31         DOWEQ     *OFF
221900931007     C*
222000931130      * TEST SE E' RECUPERO
222100950111     C                   Z-ADD     1             WA
222200950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
222300950111     C   32              MOVEL     D3A(WA)       DS3A
222400950111     C  N32              CLEAR                   DS3A
222500931006     C*
222600931006     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
222700970130    2C     §3ARBL        IFNE      'R'
222800931007     C                   EXSR      CSPETR
222900970130    2C                   ENDIF
223000931006     C*
223100950111     C*
223200931006     C*LETTURA DENTRO AL CICLO
223300020823     C     KDTA          READE     FNBTP15L                               31
223400970130    1C                   ENDDO
223500931006     C*
223600931006     C                   ENDSR
223700931007     C*****************************************************************
223800931007     C* SUBROUTINE DTBOT: NON DIFFERENZIA TRA FILIALE E C/SERVIZI
223900931007     C*****************************************************************
224000931007     C     DTBOT         BEGSR
224100950111     C*
224200931007     C*TEST SE E' C/SERVIZI O FILIALE
224300931007     C*MI SERVE SOLO PER SAPERE SE ADDIZIONARE O MENO IL PESO TASSABILE.
224400931007     C     §3ARBL        IFEQ      'C'
224500931007     C                   SETON                                        04
224600931007     C                   ELSE
224700931007     C                   SETOFF                                       04
224800931007     C                   ENDIF
224900931007     C*
225000931007     C*FISSO GLI INDICI DI SCHIERA....
225100931007     C*
225200931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
225300950111     C     BTPTSP        IFEQ      'E'
225400931007     C                   Z-ADD     2             X
225500950111     C                   ELSE
225600931007     C*
225700950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
225800950111     C     BTPTSP        IFEQ      'D'
225900931007     C                   Z-ADD     3             X
226000950111     C                   ELSE
226100950111     C                   Z-ADD     1             X
226200931007     C                   ENDIF
226300950111     C                   ENDIF
226400931007     C*
226500931007     C                   ENDSR
226600950111     C************************************************************
226700950111     C* SUBROUTINE DATBOT: REPERIMENTO DATI BOLLA PER CALCOLI (TRANSITI)
226800950111     C*****************************************************************
226900950111     C     DATBOT        BEGSR
227000021213     ***C*
227100021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
227200021213     ***C     BTPPKB        DIV(H)    BTPNCL        PESCOL            8 1
227300021213     *** *
227400021213     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
227500021213     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
227600021213     *** * BTPVLC=Volume rilevato)
227700050111     ***C     BTPNCR        IFEQ      BTPNCL
227800021213     ***C                   Z-ADD     BTPVLC        BTPVLB
227900021213     ***C                   ELSE
228000021213     ***C     BTPVLC        IFGE      BTPVLB
228100021213     ***C                   Z-ADD     BTPVLC        BTPVLB
228200021213     ***C                   ENDIF
228300021213     ***C                   ENDIF
228400021213     ***C*
228500021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
228600021213     ***C     BTPVLB        DIV(H)    BTPNCL        VOLUM            12 4
228700021220      *
228800021220      * Verifico i dati misurati con la macchina smistacolli
228900021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
229000021220      * e calcolo il VOLUME unitario medio per collo da usare se non
229100021220      * tutti i colli sono stati volumati dalla smistacolli
229200021220      *
229300030213      **** Verifico se sono stati volumati tutti i colli
229400030213     c***                clear                   Wsivol
229500021220     c                   clear                   Wvolu
229600050111     c                   IF        BTPncr = BTPncl
229700021220     c                   z-add     BTPvlc        Wvolu
229800030213     c***                eval      Wsivol =  'S'
229900021220   x1c                   else
230000021220if  2c                   if        BTPvlc >= BTPvlb
230100021220     c                   z-add     BTPvlc        Wvolu
230200021220   x2c                   else
230300021220     c                   z-add     BTPvlb        Wvolu
230400021220e   2c                   endif
230500021220e   1c                   endif
230600021220      *
230700021220      * Calcolo il volume medio
230800021220     c     Wvolu         div(H)    BTPncl        wVOLUM
230900021220      *
231000021220      * Verifico i dati misurati con la macchina smistacolli
231100021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
231200021220      * e calcolo il PESO   unitario medio per collo da usare se non
231300021220      * tutti i colli sono stati pesati   dalla smistacolli
231400021220      *
231500030213      **** Verifico se sono stati pesati   tutti i colli
231600030213     c***                clear                   Wsipes
231700021220     c                   clear                   Wpeso
231800050111     c                   If        BTPncp = BTPncl
231900021220     c                   z-add     BTPpkc        Wpeso
232000030213     c***                eval      Wsipes =  'S'
232100021220   x1c                   else
232200021220if  2c                   if        BTPpkc >= BTPpkb
232300021220     c                   z-add     BTPpkc        Wpeso
232400021220   x2c                   else
232500021220     c                   z-add     BTPpkb        Wpeso
232600021220e   2c                   endif
232700021220e   1c                   endif
232800021220      *
232900021220      * Calcolo il peso medio
233000021220     c     Wpeso         div(H)    BTPncl        wPESCOL
233100950111     C*
233200950111     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
233300021213     ***C     BTPPKB        IFLE      3500
233400021213     C     Wpeso         IFLE      3500
233500950111     C                   SETON                                        06
233600950111     C                   ELSE
233700950111     C                   SETOFF                                       06
233800950111     C                   ENDIF
233900950111     C*
234000950111     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
234100021213     ***C                   Z-ADD     BTPPKB        PESTAX            8 0
234200021213     C                   z-add     Wpeso         PESTAX
234300950111     C*
234400950111     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
234500021213     ***C     BTPVLB        MULT      §QTKPM        COMPKT
234600021213     C     Wvolu         MULT      §QTKPM        COMPKT
234700950111     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
234800950111     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
234900950111     C     COMPKT        IFGT      PESTAX
235000950111     C                   Z-ADD     COMPKT        PESTAX
235100950111     C                   END
235200950111     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
235300950111     C                   Z-ADD     0             COMPKT            8 0
235400950111     C     PESTAX        DIV       §3IARR        COMPKT
235500950111     C                   MVR                     RESTO             3 0
235600950111     C     RESTO         IFGT      0
235700950111     C     COMPKT        MULT      §3IARR        PESTAX
235800950111     C                   ADD       §3IARR        PESTAX
235900950111     C                   ENDIF
236000950111     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
236100021213     C     PESTAX        DIV(H)    BTPNCL        PESTAS
236200950111     C*
236300950111     C                   ENDSR
236400931007     C****************************************************************
236500931007     C* SUBROUTINE CALCTR:
236600931007     C* SOMMA VALORI COLLO, IN TRANSITI.
236700931007     C* COLLI;COLLI NON SPUNT;PESO;PESO TASSABILE;VOLUME.
236800931007     C*****************************************************************
236900931007     C     CALCTR        BEGSR
237000950111     C*
237100931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
237200931007     C******COLLI
237300931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
237400931007     C                   ADD       1             XKA(X)
237500931007     C*
237600931007     C******COLLI NON SPUNTATI IN PARTENZA: 88.
237700950111     C     BTTPUT        IFEQ      88
237800950331     C     BTTPUT        OREQ      89
237900970430     C     BTTPUT        OREQ      98
238000970430     C     BTTPUT        OREQ      96
238100941012     C  N04              ADD       1             TRPI88            7 0
238200931007     C                   ENDIF
238300950622     C******COLLI PISTOLA 87 (EX 88 E 89)
238400950622     C     BTTPUT        IFEQ      87
238500950622     C  N04              ADD       1             TRPI87            7 0
238600950622     C                   ENDIF
238700931007     C*
238800931007     C******PESO
238900931007     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
239000030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
239100030213      * Peso medio
239200030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
239300030213if  1c***                if        Wsipes = 'S'
239400030213     c***                z-add(H)  BTTpuc        PESCOL
239500030213   x1c***                else
239600021220     c                   z-add     Wpescol       PESCOL
239700030213e   1c***                endif
239800950403     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
239900931007     C   06              ADD       PESCOL        XKD(X)
240000931007     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
240100931007     C  N06              ADD       PESCOL        XKE(X)
240200931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
240300931007     C                   ADD       PESCOL        XKF(X)
240400931007     C*
240500931007     C*
240600931007     C******PESO TASSABILE
240700931007     C  N04
240800931007     CAN 06              ADD       PESTAS        TRPETS            9 1
240900931007     C*
241000931007     C*
241100931007     C******VOLUME
241200021213     ***C     BTTVUC        IFGT      0
241300021213     ***C                   Z-ADD(H)  BTTVUC        WVOLUM
241400021213     ***C                   ELSE
241500021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
241600021213     ***C                   ENDIF
241700021220     ***C*
241800021220     ***C                   ADD       WVOLUM        XKI(X)
241900030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
242000030213      * Volume medio
242100030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
242200030213if  1c***                if        Wsivol = 'S'
242300030213     c***                z-add     BTTvuc        Volum
242400030213   x1c***                else
242500021220     c                   z-add     Wvolum        Volum
242600030213e   1c***                endif
242700021220     c                   add       VOLUM         XKI(X)
242800931007     C*
242900931007     C                   ENDSR
243000930929     C****************************************************************
243100931007     C* SUBROUTINE CSPETR:  CALCOLO SPEDIZIONI -TRANSITI
243200931007     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
243300931007     C*****************************************************************
243400931007     C     CSPETR        BEGSR
243500931007     C*
243600931007     C*FISSO GLI INDICI DI SCHIERA....
243700931007     C*
243800931007     C* SE TIPO SPEDIZIONE E' ESPRESSO
243900950111     C     BTPTSP        IFEQ      'E'
244000931007     C                   Z-ADD     2             X
244100950111     C                   ELSE
244200931007     C*
244300950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
244400950111     C     BTPTSP        IFEQ      'D'
244500931007     C                   Z-ADD     3             X
244600950111     C                   ELSE
244700950111     C                   Z-ADD     1             X                 1 0
244800931007     C                   ENDIF
244900950111     C                   ENDIF
245000931007     C*
245100931007     C*
245200931007     C******SPEDIZIONI
245300931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
245400931007     C* SOMMO SOLO IN SCHIERA DEL TOTALE
245500931007     C                   ADD       1             XKL(X)
245600931007     C*
245700931007     C                   ENDSR
245800931007     C****************************************************************
245900931007     C* SUBROUTINE TOTTRA:
246000931007     C**CALCOLO TOTALI TRANSITI STAMPA DETTAGLIO TRANSITI
246100931007     C*****************************************************************
246200931007     C     TOTTRA        BEGSR
246300950111     C*
246400931007     C**** **** **** **** **** **** **** **** ****
246500970218     C* AGGIUNGO I COLLI IN DISGUIDO AL TOTALE COLLI CORRIERE
246600970218     C                   ADD       WDIS          XKA(1)
246700000616     C                   CLEAR                   WDIS
246800931007     C****CALCOLO TOTALE COLLI:
246900931007     C     XKA(1)        ADD       XKA(2)        XKA123            7 0
247000931008     C                   ADD       XKA(3)        XKA123
247100950331     C*
247200931007     C**** **** **** **** **** **** **** **** ****
247300931007     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
247400931007     C     XKA123        IFNE      0
247500931007     C     TRPI88        DIV(H)    XKA123        $XX
247600931007     C     $XX           MULT      100           TRPC88            5 2
247700931007     C                   ENDIF
247800931007     C*
247900931008     C     TRPI88        IFNE      0
248000931007     C                   MOVE      '%'           PER3              1
248100931007     C                   ELSE
248200931007     C                   MOVE      *BLANKS       PER3
248300931007     C                   ENDIF
248400950622     C****CALCOLO % COLLI PISTOLA 87 (EX 88 E 89)
248500950622     C     XKA123        IFNE      0
248600950622     C     TRPI87        DIV(H)    XKA123        $XX
248700950622     C     $XX           MULT      100           TRPC87            5 2
248800950622     C                   ENDIF
248900950622     C*
249000950622     C     TRPI87        IFNE      0
249100950622     C                   MOVE      '%'           PER34             1
249200950622     C                   ELSE
249300950622     C                   MOVE      *BLANKS       PER34
249400950622     C                   ENDIF
249500931007     C**** **** **** **** **** **** **** **** ****
249600931007     C****CALCOLO TOTALI PESO:
249700931007     C*
249800931007     C*TOTALE PESO FINO A 35 QUINTALI
249900931007     C     XKD(1)        ADD       XKD(2)        XKD123            8 1
250000931007     C                   ADD       XKD(3)        XKD123
250100931007     C*TOTALE PESO OLTRE  35 QUINTALI
250200931007     C     XKE(1)        ADD       XKE(2)        XKE123            8 1
250300931007     C                   ADD       XKE(3)        XKE123
250400931007     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
250500931007     C     XKF(1)        ADD       XKF(2)        XKF123            8 1
250600931007     C                   ADD       XKF(3)        XKF123
250700931007     C**** **** **** **** **** **** **** **** ****
250800931007     C****CALCOLO TOTALE VOLUME
250900941005     C     XKI(1)        ADD       XKI(2)        XKI123            9 4
251000931007     C                   ADD       XKI(3)        XKI123
251100931007     C**** **** **** **** **** **** **** **** ****
251200931007     C****CALCOLO TOTALE SPEDIZIONI
251300931007     C     XKL(1)        ADD       XKL(2)        XKL123            7 0
251400931007     C                   ADD       XKL(3)        XKL123
251500931007     C**** **** **** **** **** **** **** **** ****
251600931007     C****STAMPA TRANSITI :DETTAGLIO E TOTALE.
251700941012     C  N36              EXCEPT    DETTRA
251800950210     C     DS1RIS        IFNE      'R'
251900941012     C                   EXSR      WRITRA
252000941012     C                   ENDIF
252100931007     C**** **** **** **** **** **** **** **** ****
252200931007     C****SOMMA CAMPI TOTALI TRANSITI IN CAMPI TOTALI TOT.GEN.
252300000616     C**  SOLO SE NON STAMPO
252400000616     C     DS1RIS        IFNE      ' '
252500931007     C                   ADD       XKA123        TKA123
252600931007     C                   ADD       XKA(1)        TKA(1)
252700931007     C                   ADD       XKA(2)        TKA(2)
252800931007     C                   ADD       XKA(3)        TKA(3)
252900931007     C*
253000970130     C                   ADD       TRPI88        TGP88T            7 0
253100970130     C                   ADD       TRPI87        TGP87T            7 0
253200931007     C*
253300931007     C                   ADD       XKD123        TKD123
253400931007     C                   ADD       XKD(1)        TKD(1)
253500931007     C                   ADD       XKD(2)        TKD(2)
253600931007     C                   ADD       XKD(3)        TKD(3)
253700931007     C*
253800931007     C                   ADD       XKE123        TKE123
253900931007     C                   ADD       XKE(1)        TKE(1)
254000931007     C                   ADD       XKE(2)        TKE(2)
254100931007     C                   ADD       XKE(3)        TKE(3)
254200931007     C*
254300931007     C                   ADD       XKF123        TKF123
254400931007     C                   ADD       XKF(1)        TKF(1)
254500931007     C                   ADD       XKF(2)        TKF(2)
254600931007     C                   ADD       XKF(3)        TKF(3)
254700931007     C*
254800931007     C                   ADD       TRPETS        TGPETS            9 1
254900931007     C*
255000931007     C                   ADD       XKI123        TKI123
255100931007     C                   ADD       XKI(1)        TKI(1)
255200931007     C                   ADD       XKI(2)        TKI(2)
255300931007     C                   ADD       XKI(3)        TKI(3)
255400931007     C*
255500931007     C                   ADD       XKL123        TKL123
255600931007     C                   ADD       XKL(1)        TKL(1)
255700931007     C                   ADD       XKL(2)        TKL(2)
255800931007     C                   ADD       XKL(3)        TKL(3)
255900000616     C                   ENDIF
256000931007     C*
256100931007     C**** **** **** **** **** **** **** **** ****
256200931008     C****PULIZIA CAMPI TRANSITO (DI TOTALE E DI DETTAGLIO)
256300931007     C*CAMPI DI TOTALE
256400931007     C                   Z-ADD     0             XKA123
256500931007     C                   Z-ADD     0             TRPI88
256600931007     C                   Z-ADD     0             TRPC88
256700950622     C                   Z-ADD     0             TRPI87
256800950622     C                   Z-ADD     0             TRPC87
256900931007     C                   Z-ADD     0             XKD123
257000931007     C                   Z-ADD     0             XKE123
257100931007     C                   Z-ADD     0             XKF123
257200931007     C                   Z-ADD     0             TRPETS
257300931007     C                   Z-ADD     0             XKI123
257400931007     C                   Z-ADD     0             XKL123
257500931007     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
257600931007     C*PER LA STAMPA).
257700931007     C                   MOVEA     *ZEROS        XKA(1)
257800931007     C*
257900931007     C                   MOVEA     *ZEROS        XKD(1)
258000931007     C*
258100931007     C                   MOVEA     *ZEROS        XKE(1)
258200931007     C*
258300931007     C                   MOVEA     *ZEROS        XKF(1)
258400931007     C*
258500931007     C                   MOVEA     *ZEROS        XKI(1)
258600931007     C*
258700931007     C                   MOVEA     *ZEROS        XKL(1)
258800931007     C*
258900931007     C                   ENDSR
259000931007     C****************************************************************
259100931008     C* SUBROUTINE TOTGEN:
259200931008     C**CALCOLO TOTALI TOT.GEN. STAMPA DETTAGLIO TOT.GEN.
259300931008     C*****************************************************************
259400931008     C     TOTGEN        BEGSR
259500950111     C*
259600931008     C**** **** **** **** **** **** **** **** ****
259700931008     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
259800931008     C     TKA123        IFNE      0
259900970130     C* TOTALE
260000970131     C     TGP88T        DIV(H)    TKA123        $XX
260100970131     C     $XX           MULT      100           TGC88T            5 2
260200931008     C                   ENDIF
260300931008     C*
260400970131     C     TGP88T        IFNE      0
260500970131     C                   MOVE      '%'           PER3              1
260600970131     C                   ELSE
260700970131     C                   MOVE      *BLANKS       PER3
260800970131     C                   ENDIF
260900950622     C****CALCOLO % COLLI CON PISTOLA 87 (EX 88 E 89)
261000950622     C     TKA123        IFNE      0
261100970130     C* TOTALE
261200970131     C     TGP87T        DIV(H)    TKA123        $XX
261300970131     C     $XX           MULT      100           TGC87T            5 2
261400950622     C                   ENDIF
261500950622     C*
261600970131     C     TGP87T        IFNE      0
261700950622     C                   MOVE      '%'           PER34             1
261800950622     C                   ELSE
261900950622     C                   MOVE      *BLANKS       PER34
262000950622     C                   ENDIF
262100931008     C**** **** **** **** **** **** **** **** ****
262200931008     C****STAMPA TOT.GEN. :DETTAGLIO E TOTALE.
262300941012     C  N36              EXCEPT    DETTOT
262400931008     C**** **** **** **** **** **** **** **** ****
262500931008     C****PULIZIA CAMPI TOT.GEN.(DI TOTALE E DI DETTAGLIO)
262600931008     C*CAMPI DI TOTALE
262700931008     C                   Z-ADD     0             TKA123
262800970131     C                   Z-ADD     0             TGP88T
262900970131     C                   Z-ADD     0             TGP87T
263000970131     C                   Z-ADD     0             TGC88T
263100970131     C                   Z-ADD     0             TGC87T
263200931008     C                   Z-ADD     0             TKD123
263300931008     C                   Z-ADD     0             TKE123
263400931008     C                   Z-ADD     0             TKF123
263500931008     C                   Z-ADD     0             TGPETS
263600931008     C                   Z-ADD     0             TKI123
263700931008     C                   Z-ADD     0             TKL123
263800931008     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
263900931008     C*PER LA STAMPA).
264000931008     C                   MOVEA     *ZEROS        TKA(1)
264100931008     C                   MOVEA     *ZEROS        TKD(1)
264200931008     C                   MOVEA     *ZEROS        TKE(1)
264300931008     C                   MOVEA     *ZEROS        TKF(1)
264400931008     C                   MOVEA     *ZEROS        TKI(1)
264500931008     C                   MOVEA     *ZEROS        TKL(1)
264600940126      *
264700941012      * Verifico se vi sono fogli viaggio aperti
264800941012     C                   EXSR      FOGVIA
264900931130      *
265000931008     C                   ENDSR
265100941012      ****************************************************************
265200941012      * Scrivo in archivio il riepilogo per filiale (totali e c/serv.)
265300941012      ****************************************************************
265400941012     C     WRIFIL        BEGSR
265500941012      *
265600941012      * ---------------------- TOTALI DI FILIALE -------------------------------
265700941012      *
265800941012     C                   Z-ADD     SIMFEL        SPAFLE
265900941012     C                   Z-ADD     COMFIL        SPALNP
266000950210     C                   Z-ADD     DS1ADA        SPADRE
266100941012     C                   MOVE      *BLANK        SPATRE
266200941012      * ---- COLLI ----
266300970130     C                   Z-ADD     SKA(5)        SPACPE
266400970130     C                   Z-ADD     SKA(4)        SPACPC
266500970130     C                   Z-ADD     SKA(6)        SPACPD
266600970129     C                   Z-ADD     PB88F         SPA88B
266700970129     C                   Z-ADD     PI88          SPA88A
266800970129     C                   Z-ADD     PB87F         SPA87B
266900970129     C                   Z-ADD     PI87          SPA87A
267000941012      * ---- PESI ----
267100941012     C                   Z-ADD     SKD(5)        SPAPFE
267200970129     C                   Z-ADD     SKD(4)        SPAPFC
267300970129     C                   Z-ADD     SKD(6)        SPAPFD
267400970129     C                   Z-ADD     SKE(5)        SPAPOE
267500970129     C                   Z-ADD     SKE(4)        SPAPOC
267600970129     C                   Z-ADD     SKE(6)        SPAPOD
267700941012     C                   Z-ADD     PETS          SPAPTA
267800941012      * ---- VOLUMI ----
267900970130     C                   Z-ADD     SKI(5)        SPAVPE
268000970130     C                   Z-ADD     SKI(4)        SPAVPC
268100970130     C                   Z-ADD     SKI(6)        SPAVPD
268200941012      * ---- SPEDIZIONI --
268300970129     C                   Z-ADD     SKL(5)        SPASPE
268400970129     C                   Z-ADD     SKL(4)        SPASPC
268500970129     C                   Z-ADD     SKL(6)        SPASPD
268600970129     C                   Z-ADD     SKM(5)        SPASDE
268700970129     C                   Z-ADD     SKM(4)        SPASDC
268800970129     C                   Z-ADD     SKM(6)        SPASDD
268900970206     C                   Z-ADD     SKO(5)        SPASFE
269000970206     C                   Z-ADD     SKO(4)        SPASFC
269100970206     C                   Z-ADD     SKO(6)        SPASFD
269200970206     C                   Z-ADD     SKP(5)        SPATFE
269300970206     C                   Z-ADD     SKP(4)        SPATFC
269400970206     C                   Z-ADD     SKP(6)        SPATFD
269500941012      *
269600941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
269700941012      * riempo il "flag di trasmissione" (SPAFTR)
269800040921     C***  §5CSPA        IFEQ      'S'
269900040921     C***                MOVE      *BLANK        SPAFTR
270000040921     C***                MOVE      *ZEROS        SPADTR
270100040921     C***                ELSE
270200040921     C***                MOVE      'N'           SPAFTR
270300040921     C***                MOVE      *HIVAL        SPADTR
270400040921     C***                ENDIF
270500040921     c                   move      *blanks       spaftr
270600040921     c                   z-add     dateu         spadtr
270700970204     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI SCRIVO RECORD CON
270800970226     C* TIPO = 'F'
270900970226     C     WFES          IFEQ      'F'
271000970226     C     SPACPE        ANDEQ     *ZEROS
271100970204     C     SPACPC        ANDEQ     *ZEROS
271200970204     C     SPACPD        ANDEQ     *ZEROS
271300970204     C                   MOVE      'F'           SPATRE
271400970204     C                   END
271500970204     C                   WRITE     FNSPA000
271600941012      *
271700941012      * --------------------- CONTO SERVIZI DI FILIALE -------------------------
271800941012      *
271900941012      * Finche sono qua (totali di filiale), scrivo anche i c/serv. di filiale
272000941012     C                   Z-ADD     SIMFEL        SPAFLE
272100941012     C                   Z-ADD     COMFIL        SPALNP
272200950210     C                   Z-ADD     DS1ADA        SPADRE
272300941012     C                   MOVE      'C'           SPATRE
272400941012      * ---- COLLI ----
272500970130     C                   Z-ADD     SKA(8)        SPACPE
272600970130     C                   Z-ADD     SKA(7)        SPACPC
272700970130     C                   Z-ADD     SKA(9)        SPACPD
272800970130     C                   Z-ADD     PB88          SPA88B
272900970130     C                   Z-ADD     PS88          SPA88A
273000970130     C                   Z-ADD     PB87          SPA87B
273100970130     C                   Z-ADD     PS87          SPA87A
273200941012      * ---- PESI ----
273300941012     C                   Z-ADD     SKD(8)        SPAPFE
273400970130     C                   Z-ADD     SKD(7)        SPAPFC
273500970130     C                   Z-ADD     SKD(9)        SPAPFD
273600970130     C                   Z-ADD     SKE(8)        SPAPOE
273700970130     C                   Z-ADD     SKE(7)        SPAPOC
273800970130     C                   Z-ADD     SKE(9)        SPAPOD
273900941012     C                   Z-ADD     *ZERO         SPAPTA
274000941012      * ---- VOLUMI ----
274100970130     C                   Z-ADD     SKI(8)        SPAVPE
274200970130     C                   Z-ADD     SKI(7)        SPAVPC
274300970130     C                   Z-ADD     SKI(9)        SPAVPD
274400941012      * ---- SPEDIZIONI --
274500970130     C                   Z-ADD     SKL(8)        SPASPE
274600970130     C                   Z-ADD     SKL(7)        SPASPC
274700970130     C                   Z-ADD     SKL(9)        SPASPD
274800970130     C                   Z-ADD     SKM(8)        SPASDE
274900970130     C                   Z-ADD     SKM(7)        SPASDC
275000970130     C                   Z-ADD     SKM(9)        SPASDD
275100970206     C                   Z-ADD     SKO(8)        SPASFE
275200970206     C                   Z-ADD     SKO(7)        SPASFC
275300970206     C                   Z-ADD     SKO(9)        SPASFD
275400970206     C                   Z-ADD     SKP(8)        SPATFE
275500970206     C                   Z-ADD     SKP(7)        SPATFC
275600970206     C                   Z-ADD     SKP(9)        SPATFD
275700941012      *
275800941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
275900941012      * riempo il "flag di trasmissione" (SPAFTR)
276000040921     C***  §5CSPA        IFEQ      'S'
276100040921     C***                MOVE      *BLANK        SPAFTR
276200040921     C***                MOVE      *ZEROS        SPADTR
276300040921     C***                ELSE
276400040921     C***                MOVE      'N'           SPAFTR
276500040921     C***                MOVE      *HIVAL        SPADTR
276600040921     C***                ENDIF
276700040921     c                   clear                   spaftr
276800040921     c                   move      dateu         spadtr
276900941012      *
277000970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI NON SCRIVO NIENTE
277100970226     C     WFES          IFEQ      'F'
277200970226     C     SPACPE        ANDEQ     *ZEROS
277300970226     C     SPACPC        ANDEQ     *ZEROS
277400970226     C     SPACPD        ANDEQ     *ZEROS
277500970226     C                   ELSE
277600970226     C                   WRITE     FNSPA000
277700970226     C                   END
277800941012      *
277900941012     C                   ENDSR
278000941012      ****************************************************************
278100941012      * Scrivo in archivio il riepilogo per transiti
278200941012      ****************************************************************
278300941012     C     WRITRA        BEGSR
278400941012      *
278500941012     C                   Z-ADD     SIMFEL        SPAFLE
278600941012     C                   Z-ADD     SIMFEL        SPALNP
278700950210     C                   Z-ADD     DS1ADA        SPADRE
278800941012     C                   MOVE      'T'           SPATRE
278900941012      * ---- COLLI ----
279000970130     C                   Z-ADD     XKA(2)        SPACPE
279100970130     C                   Z-ADD     XKA(1)        SPACPC
279200970130     C                   Z-ADD     XKA(3)        SPACPD
279300970130     C* N.B.:Nei transiti non si distinguono i colli disk B dagli altri
279400970130     C                   MOVE      *ZEROS        SPA88B
279500970130     C                   Z-ADD     TRPI88        SPA88A
279600970130     C                   MOVE      *ZEROS        SPA87B
279700970130     C                   Z-ADD     TRPI87        SPA87A
279800941012      * ---- PESI ----
279900941012     C                   Z-ADD     XKD(2)        SPAPFE
280000970203     C                   Z-ADD     XKD(1)        SPAPFC
280100970203     C                   Z-ADD     XKD(3)        SPAPFD
280200970130     C                   Z-ADD     XKE(2)        SPAPOE
280300970130     C                   Z-ADD     XKE(1)        SPAPOC
280400970130     C                   Z-ADD     XKE(3)        SPAPOD
280500941012     C                   Z-ADD     TRPETS        SPAPTA
280600941012      * ---- VOLUMI ----
280700970130     C                   Z-ADD     XKI(2)        SPAVPE
280800970130     C                   Z-ADD     XKI(1)        SPAVPC
280900970130     C                   Z-ADD     XKI(3)        SPAVPD
281000941012      * ---- SPEDIZIONI --
281100970130     C                   Z-ADD     XKL(2)        SPASPE
281200970130     C                   Z-ADD     XKL(1)        SPASPC
281300970130     C                   Z-ADD     XKL(3)        SPASPD
281400970130     C                   Z-ADD     *ZERO         SPASDE
281500970130     C                   Z-ADD     *ZERO         SPASDC
281600970130     C                   Z-ADD     *ZERO         SPASDD
281700970130     C                   Z-ADD     *ZERO         SPASFE
281800970130     C                   Z-ADD     *ZERO         SPASFC
281900970130     C                   Z-ADD     *ZERO         SPASFD
282000941012      *
282100941012      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
282200941012      * riempo il "flag di trasmissione" (SPAFTR)
282300040921     C***  §5CSPA        IFEQ      'S'
282400040921     C***                MOVE      *BLANKS       SPAFTR
282500040921     C***                MOVE      *ZEROS        SPADTR
282600040921     C***                ELSE
282700040921     C***                MOVE      'N'           SPAFTR
282800040921     C***                MOVE      *HIVAL        SPADTR
282900040921     C***                ENDIF
283000040921     c                   clear                   spaftr
283100040921     c                   z-add     dateu         spadtr
283200941012      *
283300970130     C     SPACPE        IFGT      *ZERO
283400970130     C     SPACPC        ORGT      *ZERO
283500970130     C     SPACPD        ORGT      *ZERO
283600970130     C                   WRITE     FNSPA000
283700941012     C                   ENDIF
283800941012      *
283900941012     C                   ENDSR
284000941012      ****************************************************************
284100970130      * Routine di RISTAMPA: viene semplicemente letto il file FNSPA00F
284200941012      * e stampato, senza alcuna elaborazione di dati.
284300941012      * E' quindi possibile ristampare piu' giorni accorpandoli insieme.
284400941012      * Le routine di RISTAMPA sono simili a quelle del pgm FNLSC2R.
284500941012      ****************************************************************
284600941012     C     RISTAM        BEGSR
284700050829     c                   setoff                                       3534
284800941012      *
284900941012      * Verifico se richiesta una specifica area
285000950210     C     DS1FEL        COMP      *ZERO                                  34
285100050829     c
285200050829     c* Se sono in filiale imposto = simfel
285300050829     c                   if        ds1fel=0 and simfel>0
285400050829     c                   eval      ds1fel=simfel
285500050829     c                   setoff                                       34
285600050829     c                   endif
285700050829     c
285800050829     c* Se devo elaborare una sola linea di partenza, leggo con
285900050829     c*  la linea
286000050829     c                   if        lin(2)=0 and lin(1)>0
286100050829     c                   eval      klin=lin(1)
286200050829     c                   eval      kdre=ds1ada
286300050829     c                   seton                                        35
286400050829     c                   endif
286500050829     c
286600020813     C                   MOVE      *ZEROS        XD
286700941012      *
286800941012     C                   Z-ADD     *ZERO         $PRIM1            1 0
286900941012     C                   Z-ADD     *ZERO         $FILPR            3 0
287000941012     C                   Z-ADD     *ZERO         $FI2PR            3 0
287100941012      *
287200050829     C  n35DS1FEL        SETLL     FNSPA01L
287300050829     C   35kspa          SETLL     FNSPA01L
287400941012     C                   DO        *HIVAL
287500050829     c* 35 on - lettura con lnp
287600050829     c                   if        *in35
287700050829     C     kspa2         READE(N)  FNSPA01L                               33
287800050829     c                   else
287900970130     C  N34DS1FEL        READE(N)  FNSPA01L                               33
288000970130     C   34              READ(N)   FNSPA01L                               33
288100050829     c                   endif
288200941012      *
288300970130     C  N33SPADRE        IFGE      DS1ADA
288400970130     C     SPADRE        ANDLE     DS1AAL
288500941012      *
288600941012      * La linea deve essere presente in LIN
288700050829     c                   if        not *in35 and *in34
288800941012     C     SPALNP        LOOKUP    LIN                                    32
288900050829     c                   else
289000050829     c                   seton                                        32
289100050829     c                   endif
289200941012      *
289300941012     C     *IN32         IFEQ      *ON
289400941012      *
289500941012     C     SPATRE        IFEQ      *BLANK
289600941012      *
289700941012      * Al primo giro memorizzo il "precedente"
289800941012     C     $PRIM1        IFEQ      *ZERO
289900941012     C                   Z-ADD     SPAFLE        $FI2PR
290000941012     C                   Z-ADD     SPALNP        $FILPR
290100941012     C                   Z-ADD     1             $PRIM1
290200941012     C                   ENDIF
290300941012      *
290400941012      * Se cambia filiale stampo i totali per filiale
290500941012     C     SPALNP        IFNE      $FILPR
290600020813     C                   ADD       1             XD
290700941012     C                   Z-ADD     $FILPR        COMFIL
290800941012     C                   MOVE      *BLANKS       DESLNP
290900941012     C     $FILPR        CHAIN     AZORG01L                           99
291000970203     C     *IN99         IFEQ      *OFF
291100970203     C     ORGDE5        IFNE      *BLANKS
291200970203     C                   MOVEL     ORGDE5        DESLNP
291300970203     C                   ELSE
291400970203     C                   MOVEL     ORGDES        DESLNP
291500970203     C                   END
291600970203     C                   END
291700941012     C                   EXSR      TOTLNP
291800941012     C                   Z-ADD     SPALNP        $FILPR
291900941012     C                   ENDIF
292000941012      *
292100941012      * Se cambia area stampo i totali per c/servizio
292200941012     C     SPAFLE        IFNE      $FI2PR
292300941012     C                   EXSR      TOTCS
292400941012     C                   EXSR      TOTARE
292500000308     C  N08              XFOOT     XKA           TOTXKA            7 0
292600000308     C     *IN08         IFEQ      *ON
292700000308     C     TOTXKA        ORGT      0
292800000308     C                   EXSR      TOTTRA
292900000308     C                   ENDIF
293000941012     C                   Z-ADD     SPAFLE        $FI2PR
293100941012     C                   ENDIF
293200000308      *
293300941012     C                   ENDIF
293400941012      *
293500941012      * Aggiorno i totali
293600941012     C                   EXSR      SERTRA
293700941012      *
293800941012     C                   ENDIF
293900941012     C                   ENDIF
294000941012      *
294100941012     C  N33              ENDDO
294200941012      *
294300941012      * Stampo l'ultimo totale filiale ed il c/servizio
294400941012     C                   Z-ADD     $FILPR        COMFIL
294500941012     C                   MOVE      *BLANKS       DESLNP
294600941012     C     $FILPR        CHAIN     AZORG01L                           99
294700970203     C     *IN99         IFEQ      *OFF
294800970203     C     ORGDE5        IFNE      *BLANKS
294900970203     C                   MOVEL     ORGDE5        DESLNP
295000970203     C                   ELSE
295100970203     C                   MOVEL     ORGDES        DESLNP
295200970203     C                   END
295300970203     C                   END
295400941012     C                   EXSR      TOTLNP
295500941012     C                   EXSR      TOTCS
295600941012     C                   EXSR      TOTARE
295700941012      *
295800941012      * Stampo il transito
295900000308     C  N08              XFOOT     XKA           TOTXKA            7 0
296000000308     C     *IN08         IFEQ      *ON
296100000308     C     TOTXKA        ORGT      0
296200000308     C                   EXSR      TOTTRA
296300000308     C                   ENDIF
296400941012      *
296500941012     C     ENDRIS        ENDSR
296600941012      *****************************************************************
296700941012      * RISTAMPA: mantengo aggiornati i totali
296800941012      *****************************************************************
296900941012     C     SERTRA        BEGSR
297000941012      *
297100941012     C     SPATRE        IFEQ      *BLANK
297200941012      * Colli
297300970130     C                   ADD       SPACPC        SKA(4)
297400970130     C                   ADD       SPACPE        SKA(5)
297500970130     C                   ADD       SPACPD        SKA(6)
297600970130     C                   ADD       SPA88B        PB88F
297700970130     C                   ADD       SPA88A        PI88
297800970130     C                   ADD       SPA87B        PB87F
297900970130     C                   ADD       SPA87A        PI87
298000941012      * Pesi
298100970130     C                   ADD       SPAPFC        SKD(4)
298200970130     C                   ADD       SPAPFE        SKD(5)
298300970130     C                   ADD       SPAPFD        SKD(6)
298400970130     C                   ADD       SPAPOC        SKE(4)
298500970130     C                   ADD       SPAPOE        SKE(5)
298600970130     C                   ADD       SPAPOD        SKE(6)
298700970130     C     SKD(4)        ADD       SKE(4)        SKF(4)
298800970130     C     SKD(5)        ADD       SKE(5)        SKF(5)
298900970130     C     SKD(6)        ADD       SKE(6)        SKF(6)
299000941012     C                   ADD       SPAPTA        PETS
299100941012      * Volumi
299200970130     C                   ADD       SPAVPC        SKI(4)
299300970130     C                   ADD       SPAVPE        SKI(5)
299400970130     C                   ADD       SPAVPD        SKI(6)
299500941012      * Spedizioni
299600970130     C                   ADD       SPASPC        SKL(4)
299700970130     C                   ADD       SPASPE        SKL(5)
299800970130     C                   ADD       SPASPD        SKL(6)
299900970130     C                   ADD       SPASDC        SKM(4)
300000970130     C                   ADD       SPASDE        SKM(5)
300100970130     C                   ADD       SPASDD        SKM(6)
300200970206     C     SPASFC        ADD       SPATFC        SKN(4)
300300970206     C     SPASFE        ADD       SPATFE        SKN(5)
300400970206     C     SPASFD        ADD       SPATFD        SKN(6)
300500941012     C                   ENDIF
300600941012      *
300700941012     C     SPATRE        IFEQ      'C'
300800941012      * Colli
300900970130     C                   ADD       SPACPC        SKA(1)
301000970130     C                   ADD       SPACPE        SKA(2)
301100970130     C                   ADD       SPACPD        SKA(3)
301200970130     C                   ADD       SPA88B        PB88CS
301300970130     C                   ADD       SPA88B        PB88
301400970130     C                   ADD       SPA88A        PI88CS
301500970130     C                   ADD       SPA88A        PS88
301600970130     C                   ADD       SPA87B        PB87CS
301700970130     C                   ADD       SPA87B        PB87
301800970130     C                   ADD       SPA87A        PI87CS
301900970130     C                   ADD       SPA87A        PS87
302000941012      * Pesi
302100970130     C                   ADD       SPAPFC        SKD(1)
302200970130     C                   ADD       SPAPFE        SKD(2)
302300970130     C                   ADD       SPAPFD        SKD(3)
302400970130     C                   ADD       SPAPOC        SKE(1)
302500970130     C                   ADD       SPAPOE        SKE(2)
302600970130     C                   ADD       SPAPOD        SKE(3)
302700970130     C     SKD(1)        ADD       SKE(1)        SKF(1)
302800970130     C     SKD(2)        ADD       SKE(2)        SKF(2)
302900970130     C     SKD(3)        ADD       SKE(3)        SKF(3)
303000941012      * Volumi
303100970130     C                   ADD       SPAVPC        SKI(1)
303200970130     C                   ADD       SPAVPE        SKI(2)
303300970130     C                   ADD       SPAVPD        SKI(3)
303400941012      * Spedizioni
303500970130     C                   ADD       SPASPC        SKL(1)
303600970130     C                   ADD       SPASPE        SKL(2)
303700970130     C                   ADD       SPASPD        SKL(3)
303800970130     C                   ADD       SPASDC        SKM(1)
303900970130     C                   ADD       SPASDE        SKM(2)
304000970130     C                   ADD       SPASDD        SKM(3)
304100970228     C     SPATFC        ADD       SPASFC        W0010
304200970228     C     SPATFE        ADD       SPASFE        W0020
304300970228     C     SPATFD        ADD       SPASFD        W0030
304400970228     C                   ADD       W0010         SKN(1)
304500970228     C                   ADD       W0020         SKN(2)
304600970228     C                   ADD       W0030         SKN(3)
304700941012     C                   ENDIF
304800941012      *
304900941012     C     SPATRE        IFEQ      'T'
305000970130     C                   ADD       SPACPC        XKA(1)
305100970130     C                   ADD       SPACPE        XKA(2)
305200970130     C                   ADD       SPACPD        XKA(3)
305300970130     C                   ADD       SPA88A        TRPI88
305400970130     C                   ADD       SPA87A        TRPI87
305500941012      * Pesi
305600970130     C                   ADD       SPAPFC        XKD(1)
305700970130     C                   ADD       SPAPFE        XKD(2)
305800970130     C                   ADD       SPAPFD        XKD(3)
305900970130     C                   ADD       SPAPOC        XKE(1)
306000970130     C                   ADD       SPAPOE        XKE(2)
306100970130     C                   ADD       SPAPOD        XKE(3)
306200970228     C     XKD(1)        ADD       XKE(1)        XKF(1)
306300970228     C     XKD(2)        ADD       XKE(2)        XKF(2)
306400970228     C     XKD(3)        ADD       XKE(3)        XKF(3)
306500941012     C                   ADD       SPAPTA        TRPETS
306600941012      * Volumi
306700970130     C                   ADD       SPAVPC        XKI(1)
306800970130     C                   ADD       SPAVPE        XKI(2)
306900970130     C                   ADD       SPAVPD        XKI(3)
307000941012      * Spedizioni
307100970130     C                   ADD       SPASPC        XKL(1)
307200970130     C                   ADD       SPASPE        XKL(2)
307300970130     C                   ADD       SPASPD        XKL(3)
307400941012     C                   ENDIF
307500941012      *
307600941012     C                   ENDSR
307700941012      ****************************************************************
307800941012      * Verifica se vi sono dei fogli viaggio con data partenza compresa
307900941012      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
308000941012      ****************************************************************
308100941012     C     FOGVIA        BEGSR
308200941012      *
308300950404     C  N36DS1RIS        IFEQ      'S'
308400941012      *
308500020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
308600020813     ***C     *LOVAL        SETLL     FNFGV04L
308700020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
308800020813     C     SIMfel        setll     FNFGV06L
308900941012     C                   DO        *HIVAL
309000020813     ***C                   READ      FNFGV04L                               31
309100020813     C     SIMfel        reade     FNFGV06L                               31
309200950210     C  N31FGVDTP        IFGE      DS1ADA
309300950210     C     FGVDTP        ANDLE     DS1AAL
309400941012     C                   SETON                                        31
309500941012     C                   EXCEPT    FVAPER
309600941012     C                   ENDIF
309700941012     C  N31              ENDDO
309800941012      *
309900941012     C                   ENDIF
310000941012      *
310100941012     C                   ENDSR
310200940103      ****************************************************************
310300941012      * ----------------?    TESTATA  DI  STAMPA    ?-----------------
310400930928     O*PRIMA RIGA SE C'E' SOLO 'DATA DA'
310500930928     OPRTF198   E   N03      TESTA            02
310600930928     O                       SK(1)               66
310700930928     O                       SK(2)              132
310800930928     O                       SK(4)              198
310900930928     O                       RSUT                20
311000941012     O                       STRIP               48
311100950210     O                       DS1GDA             113 '  /  /    '
311200950210     O                       WDATE              181 '  /  /    '
311300930928     O                       PAGE          Z    198
311400930928     O*PRIMA RIGA SE C'E' 'DATA DA' E 'DATA A'
311500930928     O          E    03      TESTA            02
311600930928     O                       SK(1)               66
311700930928     O                       SK(3)              132
311800930928     O                       SK(4)              198
311900930928     O                       RSUT                20
312000930928     O                       SIMFEL              24
312100941012     O                       STRIP               48
312200950210     O                       DS1GDA             113 '  /  /    '
312300950210     O                       DS1GAL             128 '  /  /    '
312400950210     O                       WDATE              181 '  /  /    '
312500930928     O                       PAGE          Z    198
312600930928     O*RIGA IN CUI C'E' SOLO UTIME
312700930928     O          E            TESTA            03
312800950111     O                       KNSIF               10
312900950111     O                       KNMUS               21
313000941012     O                       STRIP1              48
313100950210     O                       UTIME              180 '  :  :  '
313200930928     O*TRATTINI
313300930928     O          E            TESTA            04
313400930928     O                       SK(5)               66
313500930928     O                       SK(5)              132
313600970306     O                       SK(18)             198
313700930928     O*PRIMA RIGA DI COSTANTI
313800930928     O          E            TESTA            05
313900930928     O                       SK(6)               66
314000970131     O                       SK(9)              132
314100970131     O                       SK(12)             198
314200930928     O*SECONDA RIGA DI COSTANTI
314300930928     O          E            TESTA            06
314400970131     O                       SK(7)               66
314500930928     O                       SK(10)             132
314600970131     O                       SK(13)             198
314700930928     O*TERZA RIGA DI COSTANTI
314800930928     O          E            TESTA            07
314900970131     O                       SK(8)               66
315000970131     O                       SK(11)             132
315100930928     O                       SK(14)             198
315200930928     O*TRATTINI
315300970131     O          E            TESTA            08
315400930928     O                       SK(5)               66
315500930928     O                       SK(5)              132
315600970306     O                       SK(18)             198
315700941012      * ----------------?    DETTAGLIO DI FILIALE    ?----------------
315800930928     O****1°RIGA DEL DETTAGLIO FILIALE = TOTALI
315900930928     O**COLLI
316000930929     O          E            DETFIL      2
316100970131     O                       DESLNP              10
316200970131     O                       SKA456        Z     18
316300970131     O*COLLI NO SPUNT. IN PART. DISK B
316400970131     O                       PB88F         Z     26
316500970131     O                       PC88B         4     33
316600970131     O                       PER3B               34
316700970131     O*COLLI NO SPUNT. IN PART. ALTRI
316800970131     O                       PI88          Z     42
316900970131     O                       PC88          4     49
317000970131     O                       PER3                50
317100970131     O*COLLI NO SPUNT. IN PART. TOTALE
317200970131     O                       PI88T         Z     58
317300970131     O                       PC88T         4     65
317400970131     O                       PER3T               66
317500970131     O*COLLI PISTOLA 87 (EX 88) DISK B
317600970131     O                       PB87F         Z     74
317700970131     O                       PC87B         4     81
317800970131     O                       PER34B              82
317900970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
318000970131     O                       PI87          Z     90
318100970131     O                       PC87          4     97
318200970131     O                       PER34               98
318300970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
318400970131     O                       PI87T         Z    106
318500970131     O                       PC87T         4    113
318600970131     O                       PER34T             114
318700930928     O**PESO
318800970131     O                       SKD456        4    124
318900970131     O                       SKE456        4    134
319000970131     O                       SKF456        4    144
319100930928     O**PESO TASSABILE
319200970131     O                       PETS          4    155
319300930928     O**VOLUME
319400970131     O                       SKI456        4    165
319500930928     O**SPEDIZIONI
319600970131     O                       SKL456        Z    174
319700970131     O                       SKM456        Z    182
319800930928     O*
319900931011     O****2°RIGA DEL DETTAGLIO FILIALE = ESPRESSO
320000930928     O**COLLI
320100930929     O          E            DETFIL      1
320200090605     O**!!!                                      11 'ESPR'
320300090605     o                       wdtsp(02)           11
320400970131     O                       SKA(5)        Z     18
320500930928     O**PESO
320600970131     O                       SKD(5)        4    124
320700970131     O                       SKE(5)        4    134
320800970131     O                       SKF(5)        4    144
320900930928     O**VOLUME
321000970131     O                       SKI(5)        4    165
321100930928     O**SPEDIZIONI
321200970131     O                       SKL(5)        Z    174
321300970131     O                       SKM(5)        Z    182
321400931011     O*
321500950111     O****3°RIGA DEL DETTAGLIO FILIALE = DISTRIBUZIONE
321600930928     O**COLLI
321700930929     O          E            DETFIL      1
321800090605     O**!!!                                      11 'DIST'
321900090605     o                       wdtsp(03)           11
322000970131     O                       SKA(6)        Z     18
322100930928     O**PESO
322200970131     O                       SKD(6)        4    124
322300970131     O                       SKE(6)        4    134
322400970131     O                       SKF(6)        4    144
322500930928     O**VOLUME
322600970131     O                       SKI(6)        4    165
322700930928     O**SPEDIZIONI
322800970131     O                       SKL(6)        Z    174
322900970131     O                       SKM(6)        Z    182
323000940323     O*
323100950111     O****4°RIGA DEL DETTAGLIO FILIALE = CORRIERE
323200940323     O**COLLI
323300940323     O          E            DETFIL      1
323400090605     O**!!!                                      11 'CORR'
323500090605     o                       wdtsp(01)           11
323600970131     O                       SKA(4)        Z     18
323700940323     O**PESO
323800970131     O                       SKD(4)        4    124
323900970131     O                       SKE(4)        4    134
324000970131     O                       SKF(4)        4    144
324100940323     O**VOLUME
324200970131     O                       SKI(4)        4    165
324300940323     O**SPEDIZIONI
324400970131     O                       SKL(4)        Z    174
324500970131     O                       SKM(4)        Z    182
324600941012      * ----------?    RIEPILOGO C/SERVIZI PER FILIALE    ?-----------
324700930929     O****1°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = TOTALI
324800930929     O**COLLI
324900970131     O          E            DETCS       2
325000970131     O                                           10 'C/SERVIZI '
325100970131     O                       SKA123        Z     18
325200970131     O*COLLI NO SPUNT. IN PART. DISK B
325300970131     O                       PB88CS        Z     26
325400970131     O                       PC88CB        4     33
325500970131     O                       PER3B               34
325600970131     O*COLLI NO SPUNT. IN PART. ALTRI
325700970131     O                       PI88CS        Z     42
325800970131     O                       PC88CS        4     49
325900970131     O                       PER3                50
326000970131     O*COLLI NO SPUNT. IN PART. TOTALE
326100970131     O                       P88CST        Z     58
326200970131     O                       PC88CT        4     65
326300970131     O                       PER3T               66
326400970131     O*COLLI PISTOLA 87 (EX 88) DISKB
326500970131     O                       PB87CS        Z     74
326600970131     O                       PC87CB        4     81
326700970131     O                       PER34B              82
326800970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
326900970131     O                       PI87CS        Z     90
327000970131     O                       PC87CS        4     97
327100970131     O                       PER34               98
327200970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
327300970131     O                       P87CST        Z    106
327400970131     O                       PC87CT        4    113
327500970131     O                       PER34T             114
327600930929     O**PESO
327700970131     O                       SKD123        4    124
327800970131     O                       SKE123        4    134
327900970131     O                       SKF123        4    144
328000930929     O**PESO TASSABILE (PER C/SERVIZI NON C'E').
328100930929     O**VOLUME
328200970131     O                       SKI123        4    165
328300930929     O**SPEDIZIONI
328400970131     O                       SKL123        Z    174
328500970131     O                       SKM123        Z    182
328600930929     O*
328700931011     O****2°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = ESPRESSO
328800930929     O**COLLI
328900930929     O          E            DETCS       1
329000090605     O**!!!                                      11 'ESPR'
329100090605     o                       wdtsp(02)           11
329200970131     O                       SKA(2)        Z     18
329300930929     O**PESO
329400970131     O                       SKD(2)        4    124
329500970131     O                       SKE(2)        4    134
329600970131     O                       SKF(2)        4    144
329700930929     O**VOLUME
329800970131     O                       SKI(2)        4    165
329900930929     O**SPEDIZIONI
330000970131     O                       SKL(2)        Z    174
330100970131     O                       SKM(2)        Z    182
330200931011     O*
330300950111     O****3°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = DISTRIBUZIONE
330400930929     O**COLLI
330500930929     O          E            DETCS       1
330600090605     O**!!!                                      11 'DIST'
330700090605     o                       wdtsp(03)           11
330800970131     O                       SKA(3)        Z     18
330900930929     O**PESO
331000970131     O                       SKD(3)        4    124
331100970131     O                       SKE(3)        4    134
331200970131     O                       SKF(3)        4    144
331300930929     O**VOLUME
331400970131     O                       SKI(3)        4    165
331500930929     O**SPEDIZIONI
331600970131     O                       SKL(3)        Z    174
331700970131     O                       SKM(3)        Z    182
331800940323     O*
331900950111     O****4°RIGA DEL DETTAGLIO FILIALE C/SERVIZI= CORRIERE
332000940323     O**COLLI
332100940323     O          E            DETCS       1
332200090605     O**!!!                                      11 'CORR'
332300090605     o                       wdtsp(01)           11
332400970131     O                       SKA(1)        Z     18
332500940323     O**PESO
332600970131     O                       SKD(1)        4    124
332700970131     O                       SKE(1)        4    134
332800970131     O                       SKF(1)        4    144
332900940323     O**VOLUME
333000970131     O                       SKI(1)        4    165
333100940323     O**SPEDIZIONI
333200970131     O                       SKL(1)        Z    174
333300970131     O                       SKM(1)        Z    182
333400941012      * ----------------?    RIEPILOGO PER AREA    ?------------------
333500941012     O*TRATTINI DI SEPARAZIONE TRA I  C/SERVIZI E L'AREA
333600941012     O          E            DETARE      1
333700941012     O                       SK(5)               66
333800941012     O                       SK(5)              132
333900970306     O                       SK(18)             198
334000930930     O****1°RIGA DEL DETTAGLIO AREA = TOTALI
334100930930     O**COLLI
334200931011     O          E            DETARE      1
334300970131     O                                           10 'TOTALE    '
334400970131     O                       AKA123        Z     18
334500970131     O*COLLI NO SPUNT. IN PART. DISK B
334600970131     O                       ARPB88        Z     26
334700970131     O                       APC88B        4     33
334800970131     O                       PER3B               34
334900970131     O*COLLI NO SPUNT. IN PART. ALTRI
335000970131     O                       ARPI88        Z     42
335100970131     O                       ARPC88        4     49
335200970131     O                       PER3                50
335300970131     O*COLLI NO SPUNT. IN PART. TOTALE
335400970131     O                       ARP88T        Z     58
335500970131     O                       APC88T        4     65
335600970131     O                       PER3T               66
335700970131     O*COLLI PISTOLA 87 (EX 88) DISK B
335800970131     O                       ARPB87        Z     74
335900970131     O                       APC87B        4     81
336000970131     O                       PER34B              82
336100970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
336200970131     O                       ARPI87        Z     90
336300970131     O                       ARPC87        4     97
336400970131     O                       PER34               98
336500970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
336600970131     O                       ARP87T        Z    106
336700970131     O                       APC87T        4    113
336800970131     O                       PER34T             114
336900930930     O**PESO
337000970131     O                       AKD123        4    124
337100970131     O                       AKE123        4    134
337200970131     O                       AKF123        4    144
337300930930     O**PESO TASSABILE
337400970131     O                       ARPETS        4    155
337500930930     O**VOLUME
337600970131     O                       AKI123        4    165
337700930930     O**SPEDIZIONI
337800970131     O                       AKL123        Z    174
337900970131     O                       AKM123        Z    182
338000930930     O*
338100931011     O****2°RIGA DEL DETTAGLIO AREA = ESPRESSO
338200930930     O**COLLI
338300930930     O          E            DETARE      1
338400090605     O**!!!                                      11 'ESPR'
338500090605     o                       wdtsp(02)           11
338600970131     O                       AKA(2)        Z     18
338700930930     O**PESO
338800970131     O                       AKD(2)        4    124
338900970131     O                       AKE(2)        4    134
339000970131     O                       AKF(2)        4    144
339100930930     O**VOLUME
339200970131     O                       AKI(2)        4    165
339300930930     O**SPEDIZIONI
339400970131     O                       AKL(2)        Z    174
339500970131     O                       AKM(2)        Z    182
339600931011     O*
339700950111     O****3°RIGA DEL DETTAGLIO AREA = DISTRIBUZIONE
339800930930     O**COLLI
339900940324     O          E            DETARE      1
340000090605     O**!!!                                      11 'DIST'
340100090605     o                       wdtsp(03)           11
340200970131     O                       AKA(3)        Z     18
340300930930     O**PESO
340400970131     O                       AKD(3)        4    124
340500970131     O                       AKE(3)        4    134
340600970131     O                       AKF(3)        4    144
340700930930     O**VOLUME
340800970131     O                       AKI(3)        4    165
340900930930     O**SPEDIZIONI
341000970131     O                       AKL(3)        Z    174
341100970131     O                       AKM(3)        Z    182
341200940323     O*
341300950111     O****4°RIGA DEL DETTAGLIO AREA = CORRIERE
341400940323     O**COLLI
341500940324     O          E            DETARE      1  1
341600090605     O**!!!                                      11 'CORR'
341700090605     o                       wdtsp(01)           11
341800970203     O                       AKA(1)        Z     18
341900940323     O**PESO
342000970203     O                       AKD(1)        4    124
342100970203     O                       AKE(1)        4    134
342200970203     O                       AKF(1)        4    144
342300940323     O**VOLUME
342400970203     O                       AKI(1)        4    165
342500940323     O**SPEDIZIONI
342600970203     O                       AKL(1)        Z    174
342700970203     O                       AKM(1)        Z    182
342800941012      * ----------------?    RIEPILOGO TRANSITI    ?------------------
342900931008     O****1°RIGA DEL DETTAGLIO TRANSITI = TOTALI
343000931008     O**COLLI
343100970203     O          E            DETTRA      1
343200970203     O                                           10 'TRANSITI  '
343300970203     O                       XKA123        Z     18
343400931008     O*COLLI NO SPUNT. IN PART.
343500970203     O                       TRPI88        Z     58
343600970203     O                       TRPC88        4     65
343700970203     O                       PER3                66
343800950622     O*COLLI PISTOLA 87 (EX 88 E 89)
343900970203     O                       TRPI87        Z    106
344000970203     O                       TRPC87        4    113
344100970203     O                       PER34              114
344200931008     O**PESO
344300970203     O                       XKD123        4    124
344400970203     O                       XKE123        4    134
344500970203     O                       XKF123        4    144
344600931008     O**PESO TASSABILE
344700970203     O                       TRPETS        4    155
344800931008     O**VOLUME
344900970203     O                       XKI123        4    165
345000931008     O**SPEDIZIONI
345100970203     O                       XKL123        Z    174
345200931008     O*
345300931011     O****2°RIGA DEL DETTAGLIO TRANSITI = ESPRESSO
345400931008     O**COLLI
345500931008     O          E            DETTRA      1
345600090605     O**!!!                                      11 'ESPR'
345700090605     o                       wdtsp(02)           11
345800970203     O                       XKA(2)        Z     18
345900931008     O**PESO
346000970203     O                       XKD(2)        4    124
346100970203     O                       XKE(2)        4    134
346200970203     O                       XKF(2)        4    144
346300931008     O**VOLUME
346400970203     O                       XKI(2)        4    165
346500931008     O**SPEDIZIONI
346600970203     O                       XKL(2)        Z    174
346700931011     O*
346800950111     O****3°RIGA DEL DETTAGLIO TRANSITI = DISTRIBUZIONE
346900931008     O**COLLI
347000931008     O          E            DETTRA      1
347100090605     O**!!!                                      11 'DIST'
347200090605     o                       wdtsp(03)           11
347300970203     O                       XKA(3)        Z     18
347400931008     O**PESO
347500970203     O                       XKD(3)        4    124
347600970203     O                       XKE(3)        4    134
347700970203     O                       XKF(3)        4    144
347800931008     O**VOLUME
347900970203     O                       XKI(3)        4    165
348000931008     O**SPEDIZIONI
348100970203     O                       XKL(3)        Z    174
348200940323     O*
348300950111     O****4°RIGA DEL DETTAGLIO TRANSITI = CORRIERE
348400940323     O**COLLI
348500940323     O          E            DETTRA      1
348600090605     O**!!!                                      11 'CORR'
348700090605     o                       wdtsp(01)           11
348800970203     O                       XKA(1)        Z     18
348900940323     O**PESO
349000970203     O                       XKD(1)        4    124
349100970203     O                       XKE(1)        4    134
349200970203     O                       XKF(1)        4    144
349300940323     O**VOLUME
349400970203     O                       XKI(1)        4    165
349500940323     O**SPEDIZIONI
349600970203     O                       XKL(1)        Z    174
349700941012      * ------------------?    TOTALI  GENERALI    ?------------------
349800940401     O*TRATTINI DI SEPARAZIONE TRA I TRANSITI ED IL TOTALE GENERALE.
349900941012     O          E            DETTOT      1
350000941012     O                       SK(5)               66
350100941012     O                       SK(5)              132
350200970306     O                       SK(18)             198
350300931008     O****1°RIGA DEL DETTAGLIO TOT.GEN. = TOTALI
350400931008     O**COLLI
350500970203     O          E            DETTOT      1
350600970203     O                                           10 'TOT.GENER.'
350700970203     O                       TKA123        Z     18
350800931008     O*COLLI NO SPUNT. IN PART.
350900970203     O                       TGP88T        Z     58
351000970203     O                       TGC88T        4     65
351100970203     O                       PER3                66
351200950622     O*COLLI PISTOLA 87 (EX 88 E 89)
351300970203     O                       TGP87T        Z    106
351400970203     O                       TGC87T        4    113
351500970203     O                       PER34              114
351600931008     O**PESO
351700970203     O                       TKD123        4    124
351800970203     O                       TKE123        4    134
351900970203     O                       TKF123        4    144
352000931008     O**PESO TASSABILE
352100970203     O                       TGPETS        4    155
352200931008     O**VOLUME
352300970203     O                       TKI123        4    165
352400931008     O**SPEDIZIONI
352500970203     O                       TKL123        Z    174
352600931008     O*
352700931011     O****2°RIGA DEL DETTAGLIO TOT.GEN. = ESPRESSO
352800931008     O**COLLI
352900931008     O          E            DETTOT      1
353000090605     O**!!!                                      11 'ESPR'
353100090605     o                       wdtsp(02)           11
353200970203     O                       TKA(2)        Z     18
353300931008     O**PESO
353400970203     O                       TKD(2)        4    124
353500970203     O                       TKE(2)        4    134
353600970203     O                       TKF(2)        4    144
353700931008     O**VOLUME
353800970203     O                       TKI(2)        4    165
353900931008     O**SPEDIZIONI
354000970203     O                       TKL(2)        Z    174
354100931008     O*
354200950111     O****3°RIGA DEL DETTAGLIO TOT.GEN. = DISTRIBUZIONE
354300931008     O**COLLI
354400931008     O          E            DETTOT      1
354500090605     O**!!!                                      11 'DIST'
354600090605     o                       wdtsp(03)           11
354700970203     O                       TKA(3)        Z     18
354800931008     O**PESO
354900970203     O                       TKD(3)        4    124
355000970203     O                       TKE(3)        4    134
355100970203     O                       TKF(3)        4    144
355200931008     O**VOLUME
355300970203     O                       TKI(3)        4    165
355400931008     O**SPEDIZIONI
355500970203     O                       TKL(3)        Z    174
355600940323     O*
355700950111     O****4°RIGA DEL DETTAGLIO TOT.GEN. = CORRIERE
355800940323     O**COLLI
355900940323     O          E            DETTOT      1
356000090605     O**!!!                                      11 'CORR'
356100090605     o                       wdtsp(01)           11
356200970203     O                       TKA(1)        Z     18
356300940323     O**PESO
356400970203     O                       TKD(1)        4    124
356500970203     O                       TKE(1)        4    134
356600970203     O                       TKF(1)        4    144
356700940323     O**VOLUME
356800970203     O                       TKI(1)        4    165
356900940323     O**SPEDIZIONI
357000970203     O                       TKL(1)        Z    174
357100931008     O*TRATTINI DI SEPARAZIONE DOPO IL TOTALE GENERALE.
357200931008     O          E            DETTOT      1  1
357300931008     O                       SK(5)               66
357400931008     O                       SK(5)              132
357500970306     O                       SK(18)             198
357600931130      *
357700941012     O          E            FINSTA           63
357800931130     O                       SK1(1)              66
357900941012     O          E            FINSTA           64
358000931130     O                       SK1(2)              66
358100931130     O                       SK1(3)             132
358200931130     O                       SK1(4)             198
358300950111     O                       §QTKPM        2     72
358400931130     O                       §3IARR        Z     99
358500941012     O          E            FINSTA           65
358600941012     O                       SK1(7)              66
358700940126      * AVVISO DI FOGLI VIAGGIO APERTI NEL PERIODO
358800941012     O          E            FVAPER      2
358900941012     O                       SK1(5)             115
359000941012     O                       SK1(6)            +  0
359100931008     O****************************************************************
359200931130      *            2         3         4         5         6         7         8
359300940406**   CMD           0123456789012345678901234567890123456789012345678901234567890
359400950209OVRPRTF FILE(PRTF198) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.part.
359500950209')
359600930928**   SK
359700050802XXXXXXXXXXXXXXXXXXXX                                                 1
359800950111   STATISTICA PARTENZE - PARTITO  IL XX/XX/XXXX                      2
359900950111   STATISTICA PARTENZE - PARTITO DAL XX/XX/XXXX  AL XX/XX/XXXX       3
360000970807                      FNLSC1R                             PAG.XXXX   4
360100930928------------------------------------------------------------------   5
360200970502  PUNTO     COLLI  |--- COLLI NON SPUNTATI (PIST.88,89,98,96) ---|
360300970204OPERATIVO            DiskB  % sul    Altri  % sul   Totale  % sul    7
360400970204                           partito         partito         partito   8
360500970131 |----- S P U N T E   P I S T O L A   87 ------| |----- P   E   S    9
360600970204   DiskB  % sul    Altri  % sul   Totale  % sul   FINO 35 OLTRE 35  10
360700970204         partito         partito         partito                    11
360800970306  O -------|     PESO      VOLUME    SPEDIZIONI                     12
360900970305 Q    TOTALE  TASSABILE            Totale  Tot.Da                   13
361000970305                                   Partito Partire                  14
3611009701311234567890 1234567 1234567 123,45% 1234567 123,45% 1234567 123,45%  15
361200970131 1234567 123,45% 1234567 123,45% 1234567 123,45% 1234567,8 1234567  16
361300970305,8 1234567,8 12345678,9 12345,6789 1234567 1234567 1234567 123,45%  17
361400970306--------------------------------------------------                  18
361500931130**
361600940405N.B.
361700940405PESO TASSABILE:  E' CALCOLATO CON UN RAPPORTO PESO/VOLUME  1 MC =
361800940405XX,XXX Q.LI ARROTONDATO AI XXXXXX KG. SUPERIORI, PER LE SOLE SPEDI
361900940405ZIONI CON PESO FINO A KG. 3500, ESCLUSE BOLLE DI RECUPERO E C/SERV
362000940126***   SONO STATI TROVATI ALCUNI FOGLI VIAGGIO APERTI AVENTI DATA P
362100940126ARTENZA COMPRESA NEL PERIODO   ***
362200940406SPEDIZIONI:  DALLA STATISTICA SONO ESLUSE LE BOLLE DI RECUPERO
