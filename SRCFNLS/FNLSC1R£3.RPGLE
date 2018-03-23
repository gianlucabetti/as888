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
002400020813     FFNFGV06L  IF   E           K DISK
002500070112     FFNBRV09L  IF   E           K DISK
002600090911     FFiSPA01L  UF A E           K DISK
002700930930     FTABEL00F  IF   E           K DISK
002800021023     FFNBLT38L  IF   E           K DISK
002900950111     FFNBLP01L  IF   E           K DISK
003000950111     F                                     RENAME(FNBLP000:FNBLP001)
003100970129     FFNBLP06L  IF   E           K DISK
003200070523     FFNBLP05L  IF   E           K DISK
003300070523     F                                     RENAME(FNBLP000:FNBLP005)
003400020823     FFNBTT38L  IF   E           K DISK
003500020823     FFNBTP11L  IF   E           K DISK
003600020823     FFNBTP15L  IF   E           K DISK
003700020823     F                                     RENAME(FNBTP000:FNBTP015)
003800990518     FFNANM04L  IF   E           K DISK
003900930924     FAZORG01L  IF   E           K DISK
004000970204     FAZCLN01L  IF   E           K DISK
004100090415     Ffista01L  uF   E           K DISK
004200021016     fTntbe01l  uf   e           k Disk
004300930927     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
004400930927     F                                     USROPN
004500931130      *--TAB.INTERNE E SCHIERE---------------------------------------*
004600950209     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
004700970306     D SK              S             66    DIM(18) CTDATA PERRCD(1)
004800941012     D SK1             S             66    DIM(7) CTDATA PERRCD(1)
004900970204     D GME             S              1    DIM(31)
005000931130      *
005100000831     D L1              S              3  0 DIM(30)
005200020418     D L6              S              3  0 DIM(30)
005300950213     D LV2             S              3  0 DIM(30)
005400000615     D LIN             S              3  0 DIM(30)
005500000615     D SVLIN           S              3  0 DIM(30)
005600950111     D E3A             S              2    DIM(60)
005700970505     D D3A             S             75    DIM(60)
005800931130      *--SCHIERE FILIALE E AREA PER CALCOLI
005900941012      * N.B.: le schiere di 6 elementi sono cosi' suddivise:
006000941012      *       .  i primi tre (1-3) elementi sono i c/servizi dell'area
006100941012      *       .  i secondi tre (4-6)sono i totali di filiale
006200941012      *       .  i terzi tre (7-9) sono i c/servizi della filiale
006300931130      *--COLLI
006400090914     D SKA             S              7  0 DIM(20)
006500090914     D AKA             S              7  0 DIM(4)
006600090914     D TKA             S              7  0 DIM(4)
006700090914     D XKA             S              7  0 DIM(4)
006800931130      *--PESO
006900090914     D SKD             S              8  1 DIM(20)
007000090914     D AKD             S              8  1 DIM(4)
007100090914     D TKD             S              8  1 DIM(4)
007200090914     D XKD             S              8  1 DIM(4)
007300090914     D SKE             S              8  1 DIM(20)
007400090914     D AKE             S              8  1 DIM(4)
007500090914     D TKE             S              8  1 DIM(4)
007600090914     D XKE             S              8  1 DIM(4)
007700090914     D SKF             S              8  1 DIM(20)
007800090914     D AKF             S              8  1 DIM(4)
007900090914     D TKF             S              8  1 DIM(4)
008000090914     D XKF             S              8  1 DIM(4)
008100931130      *--VOLUME
008200090914     D SKI             S              9  4 DIM(20)
008300090914     D AKI             S              9  4 DIM(4)
008400090914     D TKI             S              9  4 DIM(4)
008500090914     D XKI             S              9  4 DIM(4)
008600931130      *--SPEDIZIONI
008700090914     D SKL             S              7  0 DIM(20)
008800090914     D AKL             S              7  0 DIM(4)
008900090914     D TKL             S              7  0 DIM(4)
009000090914     D XKL             S              7  0 DIM(4)
009100090914     D SKM             S              7  0 DIM(20)
009200090914     D AKM             S              7  0 DIM(4)
009300090914     D SKN             S              7  0 DIM(20)
009400090914     D AKN             S              7  0 DIM(4)
009500090914     D SKO             S              7  0 DIM(20)
009600090914     D SKP             S              7  0 DIM(20)
009700020418
009800020418     d sav_t06         S              3  0 dim(30)
009900090605
010000090605     d wdtsp           s              5    dim(10)
010100020418
010200020418     d wc              s              3  0
010300021016
010400021016     d kTbeCod         s                   Like(TbeCod)
010500021016     d kTbeKe1         s                   Like(TbeKe1)
010600050829     d klin            s                   Like(ds1lnp)
010700050829     d kdre            s                   Like(spadre)
010800020418
010900931130      *--DS*---------------------------------------------------------*
011000931130      *
011100931130      * DS PER TESTARE QUANDO CAMBIA BOLLA (SI VALORIZZA LEGGENDO I SEGNACOLLI).
011200930930     D                 DS
011300950111     D  BLTAAS                 1      4  0
011400950111     D  BLTLNP                 5      7  0
011500950111     D  BLTNRS                 8      9  0
011600950111     D  BLTNSP                10     16  0
011700950111     D  DSTXT                  1     16  0
011800950111     D                 DS
011900950111     D  BTTAAS                 1      4  0
012000950111     D  BTTLNP                 5      7  0
012100950111     D  BTTNRS                 8      9  0
012200950111     D  BTTNSP                10     16  0
012300950111     D  DSTXT1                 1     16  0
012400931130      *
012500931130      * DS PER TESTARE SE BOLLETTATO IN PERIODO O IN PERIODO PRECEDENTE.
012600930930     D                 DS
012700941012     D  BLPAAS                 1      4  0
012800941012     D  BLPMGS                 5      8  0
012900941012     D  BLPDS                  1      8  0
013000040921      *
013100040921     D WLBDAT          DS                  INZ
013200040921     D  G02DAT                 1      8  0
013300040921     D  G02INV                 9     16  0
013400040921     D  G02ERR                17     17
013500040921     D  G02TGI                18     22  0
013600921222     D DS3A          E DS
013700950111     D* N.B.: SE VARIA LA LUNGHEZZA DI DS3A, MODIF.LA SCHIERA D3A
013800930927     D DS3I          E DS
013900921222     D DS3L          E DS
014000970707     D DS5P          E DS
014100950111     D DSQT1         E DS
014200950210     D DSLS01        E DS
014300970204     D  ANNODA                41     44  0
014400970204     D  MESEDA                45     46  0
014500970204     D  $GI                   47     48  0
014600920930     D CNCR80        E DS
014700920930     D KPJBA         E DS
014800920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014900950209     D* DS PER TRUL06R - CARICAMENTO £X
015000950209     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015100950314     D  T06                    1     90  0
015200950314     D                                     DIM(30)
015300970129      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015400970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
015500000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
015600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
015700021016      * Ds date statistiche
015800021016     d dSdf          e ds
015900070523     D* DS PER LETTURA TABELLA "VPO" record DECO
016000070523     D Dvpodeco      E DS
016100090605
016200090605     d ds5e          e ds
016300020813      *
016400020813      * DEFINIZIONE CAMPI
016500020813      *
016600020813     D SAVRIS          S                   LIKE(DS1RIS) inz
016700020813     D WCAA            S                   LIKE(ANMCAA) inz
016800021220     D WVOLUM          S                   LIKE(VOLUM)  inz
016900021220     D WPESCOL         S                   LIKE(PESCOL) inz
017000020813     D KDUC            S                   LIKE(BLPDUC) inz
017100020813     D KDBR            S                   LIKE(BLPDBR) inz
017200070523     D KDrt            S                   LIKE(BLPDrt) inz
017300020813     D $TFA            S                   LIKE(BLPTFA) inz
017400020813     D W0010           S                   LIKE(SPASFC) inz
017500020813     D W0020           S                   LIKE(SPASFC) inz
017600020813     D W0030           S                   LIKE(SPASFC) inz
017700090914     D W0040           S                   LIKE(SPASFh) inz
017800020813     D KNPG            S                   LIKE(BRVNPG) inz
017900020813     D SAVLNP          S                   LIKE(BLPLNP) inz
018000020813     D*
018100020813     D KCOD            S                   LIKE(TBLCOD) inz
018200020813     D KKEY            S                   LIKE(TBLKEY) inz
018300020813     D COMFIL          S                   LIKE(ORGFIL) inz
018400020813     D FLDTXT          S                   LIKE(DSTXT)  inz
018500090415     D ktrc            S                   LIKE(statrc) inz('P')
018600090415     D kfil            S                   LIKE(stafil) inz(000)
018700021213      *
018800021213     d Volum           s             12  4 inz
018900021213     d PesCol          s              8  1 inz
019000021213     d PesTax          s              8  0 inz
019100021213     d PesTas          s              9  1 inz
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
025300090911     C     FISPAK        KLIST
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
032800020212     C*
032900020212     C* Reperimento parametri aziendali
033000020212     C                   CALL      'X§PARUT'
033100020212     C                   PARM                    UT§DSE
033200020212     C                   MOVEL     RAGUT         RSUT             20
033300020212     C                   MOVEL     REC80         CNCR80
033400020212     C*
033500020212     C* Chiave di accesso per il file tabelle
033600020212     C     KTAB1         KLIST
033700020212     C                   KFLD                    CODUT
033800020212     C                   KFLD                    KCOD
033900020212     C                   KFLD                    KKEY
034000020212     C     KTAB2         KLIST
034100020212     C                   KFLD                    CODUT
034200020212     C                   KFLD                    KCOD
034300020212     C*
034400020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
034500020212     C*
034600020212     C* DETERMINO DATA E ORA DI ESECUZIONE
034700020212     C                   TIME                    WTIME            14 0
034800020212     C                   MOVE      WTIME         WDATE             8 0
034900020212     C                   MOVEL     WTIME         UTIME             6 0
035000040921     c*
035100040921     C                   CLEAR                   WLBDAT
035200040921     C                   MOVE      Wtime         G02DAT
035300040921     C                   CALL      'XSRDA8'
035400040921     C                   PARM                    WLBDAT
035500040921     C                   Z-ADD     G02INV        dateu             8 0
035600090914     c
035700090914     c* Accendo indicatore 10 de devo stampare anche tipo servizio H
035800090916     c                   if        ds1aal>=§vpotsph
035900090914     c                   seton                                        10
036000090914     c                   endif
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
071200090911     c*  della affidabilità partenza   e tipo servizio H
071300070523     C                   MOVEL     'VPO'         kTBECOD
071400070523     C                   MOVEL(P)  'DECO'        kTBEKE1
071500070523     C     KTBE          CHAIN(N)  TNTBE01L                           21
071600070523     C  N21              MOVEL     TBEUNI        DVPODECO
071700070523     C*
071800090605      * carico le descrizioni fisse dei tipi servizio
071900090605      * chiodo l'indice di schiera per ogni tipo servizio
072000090911      * 1=C - 2=E - 3=D  4=H
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
074000090911     c                   if        tblkey = 'H'
074100090911     c                   eval      wdtsp(04) = §5ed05
074200090911     c                   endif
074300090605     c                   enddo
074400090914     c
074500090914     c
074600090605
074700930927     C                   ENDSR
074800930927     C*****************************************************************
074900930924     C* SUBROUTINE LEGGI: CICLO PRINCIPALE
075000930924     C*****************************************************************
075100920930     C     LEGGI         BEGSR
075200000615     C                   CLEAR                   SAVRIS
075300941012      *
075400941012      * Se sono nelle stampe giornaliere non voglio diciture
075500950210     C     DS1FGS        IFEQ      *ZERO
075600941012     C     SIMFEL        ANDNE     *ZERO
075700941012     C                   MOVE      *BLANKS       STRIP
075800941012     C                   MOVE      *BLANKS       STRIP1
075900941012     C                   ENDIF
076000941012      *
076100970129      * Cancello dal file statistiche i record della giornata che vado
076200000614      * a elabor. ELABORO SEMPRE TUTATA LA L1 E STAMPO LIN
076300000614      * D'ORA IN AVANTI IL PGM SARA' SEMRPE CHIAMATO SEPARATAMENTE
076400000614      * PERELABORARE E PER STAMPARE
076500950210     C     DS1RIS        IFNE      'R'
076600000614     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
076700000614     C                   CLEAR                   KPJBU
076800000614     C                   CLEAR                   DSUL06
076900000614     C                   MOVE      '£1'          D06COD
077000000614     C                   MOVEL     SIMFEL        D06KEY
077100050802     C                   MOVE      'S'           D06KEY
077200050802     C                   MOVEL     ds1ada        D06drf
077300000614     C                   MOVEL     DSUL06        KPJBU
077400000614     C                   CALL      'TRUL06R'
077500000614     C                   PARM                    KPJBA
077600000614     C                   MOVEL     KPJBU         DSUL06
077700000615     C**
077800000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
077900000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
078000000615     C*  CHE USO ALLA FINE
078100000615     C     DS1RIS        IFNE      ' '
078200000615     C                   MOVEA     LIN           SVLIN
078300000615     C                   MOVEL     DS1RIS        SAVRIS
078400000615     C                   CLEAR                   DS1RIS
078500000615     C                   ENDIF
078600000615     C*
078700000615     C                   MOVEA     T06           LIN
078800941012      *
078900941012     C                   Z-ADD     1             XX                3 0
079000000614     C     LIN(XX)       DOWGT     *ZERO
079100970129      *
079200090911     C     FISPAK        SETLL     FISPA01L
079300090911     C     FISPAK        READE     FISPA01L                               99
079400940509     C*
079500940509     C     *IN99         DOWEQ     *OFF
079600090911     C                   DELETE    FISPA000
079700090911     C     FISPAK        READE     FISPA01L                               99
079800941012     C                   ENDDO
079900940509      *
080000941012     C                   ADD       1             XX
080100941012     C                   ENDDO
080200941012     C                   ENDIF
080300950209      *
080400950209      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
080500950209     C                   MOVEA     £MOD          CMD(33)
080600950209     C                   MOVEA     £OUTQ         CMD(51)
080700931130      *
080800931130      * SE NON VA BENE L'OVERRIDE, IL PGM VA A FINE.
080900950209     C                   Z-ADD     82            LUNG             15 5
081000950209     C                   MOVEL     *BLANKS       COMMAN           82
081100931130     C                   MOVEA     CMD           COMMAN
081200950209     C                   CALL      'QCMDEXC'                            99
081300920930     C                   PARM                    COMMAN
081400920930     C                   PARM                    LUNG
081500940509     C   99              GOTO      ENDLEG
081600931130      *
081700020813     C* CONTATORE 'XD': USATO PER IL CONTEGGIO FILIALI PER SALTO PAGINA.
081800020813     C                   Z-ADD     1             XC                3 0
081900020813     C                   Z-ADD     0             XD                3 0
082000941012     C  N36              OPEN      PRTF198
082100931013     C****STAMPA TESTATA
082200941012     C  N36              EXCEPT    TESTA
082300970226     C                   SETOFF                                       OF
082400000615     C*
082500000615     C* SE CLEARATO DS1RIS ACCENDO INDICATORE
082600000615     C     DS1RIS        IFEQ      ' '
082700000615     C                   SETON                                        36
082800000615     C                   ENDIF
082900940509     C* SE IN LIN,1 C'E' IL 1°LVL, (08) ELABORO I TRANSITI.
083000940509     C     SIMFEL        LOOKUP    LIN                                    08
083100950403     C*
083200970228     C                   CLEAR                   WDIS
083300970505     C                   CLEAR                   WDIS88
083400970228     C*
083500950403     C     *IN08         IFEQ      *ON
083600970228     C     DS1RIS        ANDNE     'R'
083700950403     C*
083800990517     C* SOMMO LE ANOMALIE DI TIPO "056-DISGUIDI" AI TRANSITI
083900990517     C                   Z-ADD     56            WCAA
084000990518     C     KANM          SETLL     FNANM000
084100990518     C     KANM          READE     FNANM000                               32
084200950403     C*
084300950403     C     *IN32         DOWEQ     *OFF
084400021023      * Deve elaborare solo le anomalie con ANMFLE della L1
084500021023     c     AnmFle        Lookup    L1                                     99
084600021023     c                   If        Not *In99
084700021023     c                   GoTo      Rileggi
084800021023     c                   EndIf
084900970305     C     ANMAIE        IFEQ      'E'
085000950403     C                   ADD       1             WDIS              6 0
085100970505     C* CERCO LA RELATIVA SPUNTA PARTENZA
085200970505     C                   Z-ADD     1             KNPG
085300070112     C     KBRV          CHAIN     FNBRV09L                           32
085400970505     C  N32BRVNPS        IFEQ      88
085500970505     C     BRVNPS        OREQ      89
085600970505     C     BRVNPS        OREQ      98
085700970505     C     BRVNPS        OREQ      96
085800970502     C                   ADD       1             WDIS88            6 0
085900970502     C                   ENDIF
086000970505     C                   ENDIF
086100950403     C*
086200021023     c     Rileggi       Tag
086300021023
086400990518     C     KANM          READE     FNANM000                               32
086500950403     C                   ENDDO
086600950403     C                   ENDIF
086700950403     C*
086800950403     C*
086900941012      * Se richiesta 'S'=Stampa elaboro gli archivi, altrimenti leggo il file
087000090911      * statistiche (FISPA00F) e lo stampo direttamente
087100950210     C     DS1RIS        IFNE      'R'
087200970204     C* VERIFICO SU CALENDARIO SE GIORNO FESTIVO E LO MEMORIZZO
087300090911     C* (MI SERVE PER SAPERE IL TIPO RECORD DA MEMORIZZARE IN FISPA)
087400970204     C                   CLEAR                   $TFA
087500970204     C                   CLEAR                   WFES
087600970204     C     KCLN          CHAIN     AZCLN01L                           32
087700970204     C     *IN32         IFEQ      *OFF
087800970204     C                   MOVEA     CLNMAT        GME
087900970204     C                   MOVE      GME($GI)      WFES              1
088000970204     C                   END
088100930924     C*****
088200930924     C*CICLO PER TUTTE LE FILIALI (LINEE DI PARTENZA)
088300020813     C     LIN(XC)       DOWNE     0
088400931011     C*
088500020813     C                   ADD       1             XD
088600930924     C*VALORIZZO IL CAMPO CHIAVE ...LINEA DI PARTENZA
088700020813     C                   MOVEL     LIN(XC)       COMFIL
088800930928     C* REPERIMENTO DECODIFICA LINEA PARTENZA
088900930928     C     COMFIL        CHAIN     AZORG                              32
089000970203     C     *IN32         IFEQ      *OFF
089100970203     C     ORGDE5        IFNE      *BLANKS
089200970203     C                   MOVEL     ORGDE5        DESLNP
089300970203     C                   ELSE
089400970203     C                   MOVEL     ORGDES        DESLNP           10
089500970203     C                   END
089600970203     C                   ELSE
089700970203     C                   MOVEL     *BLANKS       DESLNP
089800970203     C                   END
089900931011     C*
090000930924     C**ELABORAZIONE FILIALI (LNP) CON MEMORIZZAZIONE DEI TOTALI
090100000831     C                   EXSR      ELALNP
090200000831     C**ELABORAZIONE ANOMALIE 105 PER LA LINEA INTERESSATA
090300000831     C                   EXSR      ELA105
090400930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
090500930924     C                   EXSR      TOTLNP
090600020813     C                   ADD       1             XC
090700930924     C                   ENDDO
090800930924     C*****
090900930924     C**CALCOLO % STAMPA DEI C/SERVIZI (CONTO SERVIZI)
091000930924     C                   EXSR      TOTCS
091100931013     C**CALCOLO % E STAMPA AREA(TUTTE LE FILIALI + C/SERVIZI).
091200930924     C                   EXSR      TOTARE
091300931006     C** PER 08 ELABORO I TRANSITI
091400931006     C   08              EXSR      ELATRA
091500930924     C**CALCOLI % E STAMPA DEI TRANSITI
091600931006     C   08              EXSR      TOTTRA
091700940103      *
091800941012     C                   ELSE
091900090911      * Ristampo dal file FISPA00F
092000941012     C                   EXSR      RISTAM
092100941012     C                   ENDIF
092200000615     C*
092300000615     C* SE DEVO ANCHE ELABORARE, LO FACCIO ORA
092400000615     C     SAVRIS        IFEQ      'S'
092500000615     C                   SETOFF                                       36
092600000615     C                   MOVEL     'R'           DS1RIS
092700000615     C                   MOVEA     SVLIN         LIN
092800000615     C                   EXSR      RISTAM
092900000615     C                   ENDIF
093000940103      *
093100930924     C**CALCOLI % E STAMPA DEI TOTALI GENERALI
093200941012      * SOLO SE STAMPATA ANCHE FILIALE DI 1° LIVELLO
093300000308      * OPPURE SE SONO IN RISTAMPA E HO STAMPATO LA RIGA DEI TRANSITI
093400000308     C     *IN08         IFEQ      *ON
093500000308     C     DS1RIS        OREQ      'R'
093600000308     C     TOTXKA        ANDGT     0
093700000308     C                   EXSR      TOTGEN
093800000308     C                   ENDIF
093900940405      *
094000941012      * Stampo la dicitura finale "N.B."
094100941012     C  N36              EXCEPT    FINSTA
094200931222      *
094300931222      *CHIUSURA FILE DI STAMPA
094400941012     C  N36              CLOSE     PRTF198
094500940223      *
094600941012      * Aggiorno la data ultima elaborazione
094700950210     C     DS1RIS        IFNE      'R'
094800021016
094900021016     c                   Eval      kTbeCod = 'SDF'
095000021016     c                   Movel(p)  Simfel        kTbeKe1
095100021016     c     Ktbe          Chain     Tntbe01l
095200021016     c                   If        %Found(Tntbe01l)
095300021016     c                   Movel     TbeUni        dSdf
095400021016     c                   If        §SdfParP = *Zeros or §SdfParP > Ds1Ada
095500021016     c                   Z-Add     Ds1Ada        §SdfParP
095600021016     c                   EndIf
095700021016     c                   If        §SdfParU < Ds1Ada
095800021016     c                   Z-Add     Ds1Ada        §SdfParU
095900021016     c                   EndIf
096000021016     c                   Movel     dSdf          TbeUni
096100021016     c                   Update    Tntbe000
096200021016     c                   EndIf
096300090415     C                   ENDIF
096400090415     c
096500090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
096600090415     c*  in FISTA
096700090415     c                   if        savris='S'
096800090415     c     ksta          delete    fista01l
096900941012     C                   ENDIF
097000940223      *
097100940509     C     ENDLEG        ENDSR
097200930924     C*****************************************************************
097300930924     C* SUBROUTINE ELALNP: ELABORAZIONE FILIALI (LINEE DI PARTENZA)
097400930924     C*****************************************************************
097500930924     C     ELALNP        BEGSR
097600950111     C*
097700931008     C                   Z-ADD     0             FLDTXT
097800931013     C*ELABORO COLLI
097900021023     C     KBLT30        SETLL     FNBLT38L
098000930924     C*
098100021023     C     KBLT30        READE     FNBLT38L                               31
098200930924     C* CICLO LETTURA COLLI
098300970130    1C     *IN31         DOWEQ     *OFF
098400931013     C* VEDERE I DATI  BOLLA SOLO AL CAMBIO BOLLA
098500970130    2C     DSTXT         IFNE      FLDTXT
098600950111     C                   Z-ADD     DSTXT         FLDTXT
098700950111     C     KBLP01        CHAIN     FNBLP01L                           32
098800931130      *
098900930927     C*SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
099000970130    3C     *IN32         IFEQ      *OFF
099100950111     C                   Z-ADD     1             WA                3 0
099200950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
099300950111     C   32              MOVEL     D3A(WA)       DS3A
099400950111     C  N32              CLEAR                   DS3A
099500931130      *
099600970505     C* REPERIMENTO DATI BOLLA
099700931007     C                   EXSR      DTBOL
099800931007     C* REPERIMENTO DATI BOLLA, PER CALCOLO.
099900930927     C                   EXSR      DATBOL
100000931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN UN RECUPERO
100100970130   X3C                   ELSE
100200930924     C                   MOVEL     'R'           §3ARBL
100300970130    3C                   ENDIF
100400970130    2C                   ENDIF
100500930924     C*
100600931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
100700970130    2C     §3ARBL        IFNE      'R'
100800930924     C*ESECUZIONE SUBROUTINE DEI CALCOLI
100900930924     C                   EXSR      CALC
101000970130    2C                   ENDIF
101100930924     C*
101200021023     C     KBLT30        READE     FNBLT38L                               31
101300970130    1C                   ENDDO
101400070523     c
101500070523     c* Se data elaborazione <= data calcolo affidabilità partenza
101600070523     c*  con data ritiro, calcolo a modo vecchio
101700070523     c                   if        ds1ada<§vponpi
101800930927     C***
101900931013     C*ELABORO SPEDIZIONI
102000970129     C**
102100970129     C** I° CICLO --> DUC = 0 DBR <= DATA ELABORAZIONE
102200970204     C*               ESCLUDENDO QUELLE CON DATA BORDERO' = 0
102300970129     C**
102400970129     C                   MOVE      *ZEROS        KDUC
102500970204     C                   MOVE      *ZEROS        KDBR
102600970204     C     KBLP6A        SETGT     FNBLP06L
102700970129     C     KBLP6         READE     FNBLP06L                               31
102800970130    1C     *IN31         DOWEQ     *OFF
102900970203     C     BLPDBR        ANDLE     DS1ADA
103000931130      * TEST SE E' RECUPERO
103100950111     C                   Z-ADD     1             WA
103200950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
103300950111     C   32              MOVEL     D3A(WA)       DS3A
103400950111     C  N32              CLEAR                   DS3A
103500930927     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
103600970203    2C     §3ARBL        IFNE      'R'
103700970304     C     BLPNFV        ANDNE     99999
103800970304     C     BLPNFV        ANDNE     99998
103900990907     C     BLPCBO        ANDNE     '£ '
104000070525     c     blppdr        andgt     0
104100931007     C                   EXSR      CSPE
104200970203    2C                   ENDIF
104300970203     C     KBLP6         READE     FNBLP06L                               31
104400970130    1C                   ENDDO
104500970129     C**
104600970206     C** II° CICLO --> DUC = DATA ELABORAZIONE
104700970129     C**
104800970218     C                   Z-ADD     DS1ADA        KDUC
104900970206     C     KBLP6         SETLL     FNBLP06L
105000970206     C     KBLP6         READE     FNBLP06L                               31
105100970130    1C     *IN31         DOWEQ     *OFF
105200970129      * TEST SE E' RECUPERO
105300970129     C                   Z-ADD     1             WA
105400970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
105500970129     C   32              MOVEL     D3A(WA)       DS3A
105600970129     C  N32              CLEAR                   DS3A
105700970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
105800970130    2C     §3ARBL        IFNE      'R'
105900970129     C                   EXSR      CSPE
106000970130    2C                   ENDIF
106100970206     C     KBLP6         READE     FNBLP06L                               31
106200970130    1C                   ENDDO
106300970129     C**
106400970129     C** III° CICLO --> DUC > DATA ELABORAZIONE; DBR <= DATA ELABORAZ.
106500970129     C**
106600970129     C                   Z-ADD     DS1ADA        BLPDUC
106700970129     C     KBLP6B        SETGT     FNBLP06L
106800970129     C     COMFIL        READE     FNBLP06L                               31
106900970130    1C     *IN31         DOWEQ     *OFF
107000970204     C*
107100970130    2C     BLPDBR        IFGT      DS1ADA
107200970129     C     KBLP6B        SETGT     FNBLP06L
107300970129     C                   GOTO      LEGBLP
107400970204     C                   ELSE
107500970204     C* ESCLUDO SE DBR = 0
107600970204     C     BLPDBR        IFEQ      *ZEROS
107700970204     C     KBLP6C        SETGT     FNBLP06L
107800970204     C                   GOTO      LEGBLP
107900970204     C                   END
108000970130    2C                   END
108100970129      * TEST SE E' RECUPERO
108200970129     C                   Z-ADD     1             WA
108300970129     C     BLPCBO        LOOKUP    E3A(WA)                                32
108400970129     C   32              MOVEL     D3A(WA)       DS3A
108500970129     C  N32              CLEAR                   DS3A
108600970129     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
108700970130    2C     §3ARBL        IFNE      'R'
108800070525     C     BLPNFV        ANDNE     99999
108900070525     C     BLPNFV        ANDNE     99998
109000070525     C     BLPCBO        ANDNE     '£ '
109100070525     c     blppdr        andgt     0
109200970129     C                   EXSR      CSPE
109300970130    2C                   ENDIF
109400970129     C     LEGBLP        TAG
109500970203     C     COMFIL        READE     FNBLP06L                               31
109600970130    1C                   ENDDO
109700070523     c
109800070523     c                   else
109900070523     c                   exsr      CALSPEDNEW
110000070523     c                   endif
110100930924     C*
110200930924     C                   ENDSR
110300070523     C*****************************************************************
110400070523     C* CALCOLO LE SPEDIZIONI A MODO NUOVO CONSIDERANDO LA DATA
110500070523     C*   RITIRO MERCE
110600070523     C*****************************************************************
110700070523     C     CALSPEDNEW    BEGSR
110800070523     C***
110900070523     C*ELABORO SPEDIZIONI
111000070523     C**
111100070523     C** I° CICLO --> DUC = 0 Drt <= DATA ELABORAZIONE
111200070523     C**
111300070523     C                   MOVE      *ZEROS        KDUC
111400070523     C                   MOVE      *ZEROS        KDrt
111500070523     C     KBLP5A        SETGT     FNBLP05L
111600070523     C     KBLP5         READE     FNBLP05L                               31
111700070523    1C     *IN31         DOWEQ     *OFF
111800070523     C     BLPDrt        ANDLE     DS1ADA
111900070523      * TEST SE E' RECUPERO
112000070523     C                   Z-ADD     1             WA
112100070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
112200070523     C   32              MOVEL     D3A(WA)       DS3A
112300070523     C  N32              CLEAR                   DS3A
112400070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
112500070523    2C     §3ARBL        IFNE      'R'
112600070523     C     BLPNFV        ANDNE     99999
112700070523     C     BLPNFV        ANDNE     99998
112800070523     C     BLPCBO        ANDNE     '£ '
112900070525     C     BLPpdr        ANDgt     0
113000070523     C                   EXSR      CSPE
113100070523    2C                   ENDIF
113200070523     C     KBLP5         READE     FNBLP05L                               31
113300070523    1C                   ENDDO
113400070523     C**
113500070523     C** II° CICLO --> DUC = DATA ELABORAZIONE
113600070523     C**
113700070523     C                   Z-ADD     DS1ADA        KDUC
113800070523     C     KBLP5         SETLL     FNBLP05L
113900070523     C     KBLP5         READE     FNBLP05L                               31
114000070523    1C     *IN31         DOWEQ     *OFF
114100070523      * TEST SE E' RECUPERO
114200070523     C                   Z-ADD     1             WA
114300070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
114400070523     C   32              MOVEL     D3A(WA)       DS3A
114500070523     C  N32              CLEAR                   DS3A
114600070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
114700070523    2C     §3ARBL        IFNE      'R'
114800070525     C     BLPNFV        ANDNE     99999
114900070525     C     BLPNFV        ANDNE     99998
115000070525     C     BLPCBO        ANDNE     '£ '
115100070523     C                   EXSR      CSPE
115200070523    2C                   ENDIF
115300070523     C     KBLP5         READE     FNBLP05L                               31
115400070523    1C                   ENDDO
115500070523     C**
115600070523     C** III° CICLO --> DUC > DATA ELABORAZIONE; Drt <= DATA ELABORAZ.
115700070523     C**
115800070523     C                   Z-ADD     DS1ADA        BLPDUC
115900070523     C     KBLP5B        SETGT     FNBLP05L
116000070523     C     COMFIL        READE     FNBLP05L                               31
116100070523    1C     *IN31         DOWEQ     *OFF
116200070523     C*
116300070523    2C     BLPDrt        IFGT      DS1ADA
116400070523     C     KBLP5B        SETGT     FNBLP05L
116500070523     C                   GOTO      LEGBLP5
116600070523   x2C                   ELSE
116700070523      * TEST SE E' RECUPERO
116800070523     C                   Z-ADD     1             WA
116900070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
117000070523     C   32              MOVEL     D3A(WA)       DS3A
117100070523     C  N32              CLEAR                   DS3A
117200070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
117300070523    3C     §3ARBL        IFNE      'R'
117400070525     C     BLPNFV        ANDNE     99999
117500070525     C     BLPNFV        ANDNE     99998
117600070525     C     BLPCBO        ANDNE     '£ '
117700070525     C     BLPpdr        ANDgt     0
117800070523     C                   EXSR      CSPE
117900070523    3C                   ENDIF
118000070523    2C                   ENDIF
118100070523     C     LEGBLP5       TAG
118200070523     C     COMFIL        READE     FNBLP05L                               31
118300070523    1C                   ENDDO
118400070523     C                   ENDSR
118500000831     C*****************************************************************
118600000831     C* ELABORO LE ANOMALIE 105 PER LA LINEA INTERESSATA
118700000831     C*****************************************************************
118800000831     C     ELA105        BEGSR
118900070523     C                   Z-ADD     105           WCAA
119000000831     C     KANM          SETLL     FNANM000
119100000831     C     KANM          READE     FNANM000                               32
119200000831     C*
119300000831     C     *IN32         DOWEQ     *OFF
119400000831     C* SE ANOMAIA CHIUSA VUOL DIRE CHE ADESSOHO LA BOLLA
119500000831     C     ANMDCH        IFEQ      0
119600040601     c     anmfle        andeq     simfel
119700000831     C* SE NON E' UGUALE ALLA LINEA DI PARTENZA, VEDO SE CONFERMABILE
119800000831     C*  DA ME
119900000831     C     ANMFLS        IFNE      COMFIL
120000000831     C* SE E' IN T06 NON MEMORIZZO, ALTRIMENTI MEMORIZZO PER SIMFEL
120100000831     C     ANMFLS        LOOKUP    T06                                    32
120200000831     C     *IN32         IFEQ      *OFF
120300000831     C     COMFIL        ANDEQ     SIMFEL
120400000831     C* NON SAPENDO IL TIPO SERVIZIO, SOMMO NELLA DISTRIBUZIONE
120500000831     C                   ADD       1             SKA(6)
120600000831     C                   ENDIF
120700000831     C*
120800000831     C                   ELSE
120900000831     C                   ADD       1             SKA(6)
121000000831     C                   ENDIF
121100000831     C                   ENDIF
121200000831     C*
121300000831     C     KANM          READE     FNANM000                               32
121400000831     C                   ENDDO
121500000831     C*
121600000831     C                   ENDSR
121700930924     C*****************************************************************
121800931007     C* SUBROUTINE DTBOL: DIFFERENZIA TRA FILIALE E C/SERVIZI
121900931013     C* ESEGUITA SOLO PER BOLLE NORMALI.
122000930927     C*****************************************************************
122100931007     C     DTBOL         BEGSR
122200000831     C*
122300970129     C* MEMORIZZO SE E' DISK B O ALTRI
122400970129     C     BLPSOP        IFEQ      '§'
122500970129     C     BLPNRS        ANDGT     *ZEROS
122600970129     C     BLPSOP        OREQ      '%'
122700970129     C     BLPNRS        ANDGT     *ZEROS
122800970129     C                   SETON                                        05
122900970129     C                   ELSE
123000970129     C                   SETOFF                                       05
123100970129     C                   END
123200090914     C
123300090914     C* FISSO GLI INDICI DI SKIERA
123400090914     C                   EXSR      INDSKIERA
123500970129     C*
123600931007     C                   ENDSR
123700090914     C************************************************************
123800090914     C* FISSO GLI INDICI DI SKIERA
123900090914     C*****************************************************************
124000090914     C     IndSkiera     BEGSR
124100090914     C*TEST SE E' C/SERVIZI O FILIALE
124200090914     C     §3ARBL        IFEQ      'C'
124300090914     C                   SETON                                        04
124400090914     C                   ELSE
124500090914     C                   SETOFF                                       04
124600090914     C                   ENDIF
124700090914     C*
124800090914     C* SE TIPO SPEDIZIONE E' H 10 30
124900090914     C                   SELECT
125000090914     C     BLPTSP        WHENEQ    'H'
125100090914     C*  C /SERVIZI
125200090914     C   04              Z-ADD     4             X
125300090914     C*
125400090914     C  N04              Z-ADD     9             X
125500090914     C
125600090914     C* SE TIPO SPEDIZIONE E' PRIORITY
125700090914     C     BLPTSP        WHENEQ    'E'
125800090914     C*  C /SERVIZI
125900090914     C   04              Z-ADD     2             X
126000090914     C*
126100090914     C  N04              Z-ADD     7             X
126200090914     C
126300090914     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
126400090914     C     BLPTSP        WHENEQ    'D'
126500090914     C*  C/SERVIZI
126600090914     C   04              Z-ADD     3             X
126700090914     C*
126800090914     C  N04              Z-ADD     8             X
126900090914     C
127000090914     C* SE TIPO SPEDIZIONE E' EXPRESS
127100090914     c                   other
127200090914     C*
127300090914     C*  C/SERVIZI
127400090914     C   04              Z-ADD     1             X                 2 0
127500090914     C*
127600090914     C  N04              Z-ADD     6             X
127700090914     C*
127800090914     C                   ENDsl
127900090914     C*
128000090914      * Preparo l'indice per c/servizio di filiale (dal 7 al 9), per le schiere
128100090914     C   04X             ADD       10            Y                 2 0
128200090914     c
128300090914     c                   ENDSR
128400931007     C************************************************************
128500931007     C* SUBROUTINE DATBOL: REPERIMENTO DATI BOLLA PER CALCOLI.
128600931007     C*****************************************************************
128700931007     C     DATBOL        BEGSR
128800021213     ***C*
128900021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
129000021213     ***C     BLPVLF        DIV(H)    BLPNCL        VOLUM            12 4
129100021213     ***C*
129200021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
129300021213     ***C     BLPPKF        DIV(H)    BLPNCL        PESCOL            8 1
129400021220      *
129500021220      * Verifico i dati misurati con la macchina smistacolli
129600021220      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
129700021220      * e calcolo il VOLUME unitario medio per collo da usare se non
129800021220      * tutti i colli sono stati volumati dalla smistacolli
129900021220      *
130000030213      **** Verifico se sono stati volumati tutti i colli
130100030213     c***                clear                   Wsivol
130200021220     c                   clear                   Wvolu
130300050111     c                   if        BLPncr = BLPncl
130400021220     c                   z-add     BLPvlc        Wvolu
130500030213     c***                eval      Wsivol =  'S'
130600021220   x1c                   else
130700021220if  2c                   if        BLPvlc >= BLPvlf
130800021220     c                   z-add     BLPvlc        Wvolu
130900021220   x2c                   else
131000021220     c                   z-add     BLPvlf        Wvolu
131100021220e   2c                   endif
131200021220e   1c                   endif
131300021220      *
131400021220      * Calcolo il volume medio
131500021220     c     Wvolu         div(H)    BLPncl        wVOLUM
131600021220      *
131700021220      * Verifico i dati misurati con la macchina smistacolli
131800021220      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
131900021220      * e calcolo il PESO   unitario medio per collo da usare se non
132000021220      * tutti i colli sono stati pesati   dalla smistacolli
132100021220      *
132200030213      **** Verifico se sono stati pesati   tutti i colli
132300030213     c***                clear                   Wsipes
132400021220     c                   clear                   Wpeso
132500050111     c                   if        BLPncp = BLPncl
132600021220     c                   z-add     BLPpkc        Wpeso
132700030213     c***                eval      Wsipes =  'S'
132800021220   c1c                   else
132900021220if  2c                   if        BLPpkc >= BLPpkf
133000021220     c                   z-add     BLPpkc        Wpeso
133100021220   x2c                   else
133200021220     c                   z-add     BLPpkf        Wpeso
133300021220e   2c                   endif
133400021220e   1c                   endif
133500021220      *
133600021220      * Calcolo il peso medio
133700021220     c     Wpeso         div(H)    BLPncl        wPESCOL
133800940328     C*
133900930927     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
134000021213     ***C     BLPPKF        IFLE      3500
134100021213     C     Wpeso         IFLE      3500
134200930927     C                   SETON                                        06
134300930927     C                   ELSE
134400930927     C                   SETOFF                                       06
134500930927     C                   ENDIF
134600930927     C*
134700931006     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
134800021213     ***C                   Z-ADD     BLPPKF        PESTAX            8 0
134900021213     C                   z-add     Wpeso         PESTAX
135000930927     C*
135100930927     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
135200021213     ***C     BLPVLF        MULT      §QTKPM        COMPKT
135300021213     C     Wvolu         MULT      §QTKPM        COMPKT
135400930927     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
135500930927     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
135600930927     C     COMPKT        IFGT      PESTAX
135700930927     C                   Z-ADD     COMPKT        PESTAX
135800930927     C                   END
135900930930     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
136000930927     C                   Z-ADD     0             COMPKT            8 0
136100930927     C     PESTAX        DIV       §3IARR        COMPKT
136200930927     C                   MVR                     RESTO             3 0
136300930927     C     RESTO         IFGT      0
136400930927     C     COMPKT        MULT      §3IARR        PESTAX
136500930927     C                   ADD       §3IARR        PESTAX
136600930930     C                   ENDIF
136700021213     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
136800021213     C     PESTAX        DIV(H)    BLPNCL        PESTAS
136900930927     C*
137000930927     C                   ENDSR
137100930927     C****************************************************************
137200930929     C* SUBROUTINE CALC:
137300930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
137400930927     C*****************************************************************
137500930927     C     CALC          BEGSR
137600930927     C*
137700930927     C******COLLI
137800930927     C                   ADD       1             SKA(X)
137900941012     C   04              ADD       1             SKA(Y)
138000930927     C*
138100930927     C******COLLI NON SPUNTATI IN PARTENZA: 88.
138200930927     C     BLTNPP        IFEQ      88
138300950328     C     BLTNPP        OREQ      89
138400970430     C     BLTNPP        OREQ      98
138500970430     C     BLTNPP        OREQ      96
138600090914     c
138700000831     C* SOLO PER LE BOLLE POSTE, CONTO ANCHE I COLLI CON NPS=90
138800090914     C**** LNPPT         OREQ      'S'
138900090914     C**** BLTNPP        ANDEQ     90
139000970129     C* DISK B
139100970129     C     *IN05         IFEQ      *ON
139200970129     C*SE (FILIALE) C/SERVIZI
139300970129     C   04              ADD       1             PB88CS            7 0
139400970129     C   04              ADD       1             PB88              7 0
139500970129     C*SE FILIALE
139600970129     C  N04              ADD       1             PB88F             7 0
139700970129     C                   ELSE
139800970129     C* NO DISK B
139900970129     C*
140000930927     C*SE (FILIALE) C/SERVIZI
140100930927     C   04              ADD       1             PI88CS            7 0
140200941012     C   04              ADD       1             PS88              7 0
140300930927     C*SE FILIALE
140400930927     C  N04              ADD       1             PI88              7 0
140500970129     C                   END
140600930927     C                   ENDIF
140700950622     C******COLLI PISTOLA 87 (EX 88)
140800950622     C     BLTNPP        IFEQ      87
140900970129     C* DISK B
141000970129     C     *IN05         IFEQ      *ON
141100970129     C*SE (FILIALE) C/SERVIZI
141200970129     C   04              ADD       1             PB87CS            7 0
141300970129     C   04              ADD       1             PB87              7 0
141400970129     C*SE FILIALE
141500970129     C  N04              ADD       1             PB87F             7 0
141600970129     C                   ELSE
141700970129     C* NO DISK B
141800970129     C*
141900950622     C*SE (FILIALE) C/SERVIZI
142000950622     C   04              ADD       1             PI87CS            7 0
142100950622     C   04              ADD       1             PS87              7 0
142200950622     C*SE FILIALE
142300950622     C  N04              ADD       1             PI87              7 0
142400970129     C                   END
142500950622     C                   ENDIF
142600930927     C*
142700930927     C******PESO
142800930927     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
142900030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
143000030213      * Peso medio
143100030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
143200030213if  1c***                if        Wsipes = 'S'
143300030213     c***                z-add(H)  BLTpuc        PESCOL
143400030213   x1c***                else
143500021220     c                   z-add     Wpescol       PESCOL
143600030213e   1c***                endif
143700930927     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
143800930927     C   06              ADD       PESCOL        SKD(X)
143900941012     C   06
144000941012     CAN 04              ADD       PESCOL        SKD(Y)
144100970129     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
144200970129     C  N06              ADD       PESCOL        SKE(X)
144300970129     C  N06
144400970129     CAN 04              ADD       PESCOL        SKE(Y)
144500930927     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
144600930927     C                   ADD       PESCOL        SKF(X)
144700941012     C   04              ADD       PESCOL        SKF(Y)
144800930927     C*
144900930927     C******PESO TASSABILE
145000931007     C  N04
145100931007     CAN 06              ADD       PESTAS        PETS              9 1
145200931130      *
145300931130      ***** VOLUME
145400021220     *** * Se nel collo c'e' il volume utilizzo quello, altrimenti quello
145500021220     *** *  generico
145600021213     ***C     BLTVUC        IFNE      *ZERO
145700021213     ***C                   Z-ADD(H)  BLTVUC        WVOLUM
145800021213     ***C                   ELSE
145900021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
146000021213     ***C                   ENDIF
146100021220     ***C                   ADD       WVOLUM        SKI(X)
146200021220     ***C   04              ADD       WVOLUM        SKI(Y)
146300021220     *** *
146400021220      * VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
146500030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
146600030213      * Volume medio
146700030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
146800030213if  1c***                if        Wsivol = 'S'
146900030213     c***                z-add     BLTvuc        Volum
147000030213   x1c***                else
147100021220     c                   z-add     Wvolum        Volum
147200030213e   1c***                endif
147300021220     c                   add       VOLUM         SKI(X)
147400021220     c   04              add       VOLUM         SKI(Y)
147500931130      *
147600930927     C                   ENDSR
147700930927     C****************************************************************
147800931007     C* SUBROUTINE CSPE:  CALCOLO SPEDIZIONI
147900930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
148000930927     C*****************************************************************
148100931007     C     CSPE          BEGSR
148200931006     C*
148300931006     C*FISSO GLI INDICI DI SCHIERA....
148400090914     c                   exsr      IndSkiera
148500970228     C*
148600970206     C* Richiamo pgm per calcolo affidabilità
148700970303     C     BLPDCE        IFEQ      *ZEROS
148800970303     C     BLPDUC        ORNE      DS1ADA
148900970206     C                   MOVEL     ' '           D98TLA
149000970206     C                   MOVEL     'P'           D98TBO
149100970206     C                   MOVEL     BLPAAS        D98AAS
149200970206     C                   MOVEL     BLPLNP        D98LNP
149300970206     C                   MOVEL     BLPNRS        D98NRS
149400970206     C                   MOVEL     BLPNSP        D98NSP
149500970310     C                   Z-ADD     DS1ADA        D98DRF
149600970310     C                   Z-ADD     2359          D98ORF
149700970925     C                   CALL      'TNSD99R'
149800970925     C                   PARM                    DS99
149900970303     C                   ELSE
150000970303     C                   Z-ADD     BLPNPI        D98NPI
150100970303     C                   END
150200970206     C*
150300930927     C******SPEDIZIONI
150400970206    1C     BLPDUC        IFEQ      DS1ADA
150500970206     C*  PARTITO
150600930927     C                   ADD       1             SKL(X)
150700941012     C   04              ADD       1             SKL(Y)
150800970206     C*  PARTITO FUORI AFFIDABILITA'
150900090911     C* (SKO LO CONTO SOLO PER MEMORIZZARLO NEL FILE FISPA)
151000970206    2C     D98NPI        IFGT      0
151100970206     C                   ADD       1             SKO(X)
151200970206     C   04              ADD       1             SKO(Y)
151300970206     C                   ADD       1             SKN(X)
151400970206     C   04              ADD       1             SKN(Y)
151500970206    2C                   END
151600970206   X1C                   ELSE
151700970206     C* A TERRA FUORI AFFIDABILITA'
151800090911     C* (SKP LO CONTO SOLO PER MEMORIZZARLO NEL FILE FISPA)
151900970206    2C     D98NPI        IFGE      0
152000970206     C                   ADD       1             SKP(X)
152100970206     C   04              ADD       1             SKP(Y)
152200970206     C                   ADD       1             SKN(X)
152300970206     C   04              ADD       1             SKN(Y)
152400970206    2C                   END
152500970206    1C                   END
152600970129     C*  DA PARTIRE
152700970206     C                   MOVE      *BLANKS       COMDAP            1
152800970228     C* La distribuzione a terra del giorno non spareggiata non viene
152900970228     C* conteggiata nel "Da partire"
153000970303     C     BLPDUC        IFEQ      *ZEROS
153100970228     C     BLPDPC        ANDEQ     0
153200970206     C     D98NPI        ANDLT     0
153300970228     C*
153400970303     C     BLPDUC        OREQ      *ZEROS
153500970228     C     BLPDPC        ANDGT     DS1ADA
153600970228     C     D98NPI        ANDLT     0
153700970228     C*
153800970303     C     BLPDUC        ORGT      DS1ADA
153900970228     C     BLPDPC        ANDGT     DS1ADA
154000970228     C     D98NPI        ANDLT     0
154100970228     C*
154200970206     C                   MOVE      'N'           COMDAP
154300970206     C                   END
154400970206     C*
154500970206     C     COMDAP        IFEQ      *BLANKS
154600970129     C                   ADD       1             SKM(X)
154700970129     C   04              ADD       1             SKM(Y)
154800970206     C                   END
154900970228     C*
155000930927     C                   ENDSR
155100930927     C****************************************************************
155200930927     C* SUBROUTINE TOTLNP:
155300930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
155400930927     C*****************************************************************
155500930927     C     TOTLNP        BEGSR
155600950111     C*
155700930927     C***CALCOLO TOTALI COLLI
155800930927     C*TOTALE COLLI
155900090914     C     SKA(6)        ADD       SKA(7)        SKA456            7 0
156000090914     C                   ADD       SKA(8)        SKA456
156100090914     C                   ADD       SKA(9)        SKA456
156200930927     C**** **** **** **** **** **** **** **** ****
156300930927     C*
156400970129     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
156500930930     C     SKA456        IFNE      0
156600970129     C* DISK B
156700970203     C     PB88F         DIV(H)    SKA456        $XX               7 4
156800970129     C     $XX           MULT      100           PC88B             5 2
156900970129     C* ALTRI
157000970129     C     PI88          DIV(H)    SKA456        $XX
157100970129     C     $XX           MULT      100           PC88              5 2
157200970129     C* TOTALE
157300970129     C     PB88F         ADD       PI88          PI88T             7 0
157400970129     C     PI88T         DIV(H)    SKA456        $XX
157500970129     C     $XX           MULT      100           PC88T             5 2
157600930930     C                   ENDIF
157700931005     C*
157800970129     C                   CLEAR                   PER3T
157900970129     C     PB88F         IFNE      0
158000970129     C                   MOVE      '%'           PER3B             1
158100970129     C                   MOVE      '%'           PER3T             1
158200970129     C                   ELSE
158300970129     C                   MOVE      *BLANKS       PER3B
158400970129     C                   ENDIF
158500970129     C*
158600931005     C     PI88          IFNE      0
158700931005     C                   MOVE      '%'           PER3              1
158800970129     C                   MOVE      '%'           PER3T             1
158900931005     C                   ELSE
159000931005     C                   MOVE      *BLANKS       PER3
159100931005     C                   ENDIF
159200950622     C*
159300950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
159400950622     C     SKA456        IFNE      0
159500970129     C* DISK B
159600970129     C     PB87F         DIV(H)    SKA456        $XX
159700970129     C     $XX           MULT      100           PC87B             5 2
159800970129     C* ALTRI
159900950622     C     PI87          DIV(H)    SKA456        $XX
160000950622     C     $XX           MULT      100           PC87              5 2
160100970129     C* TOTALI
160200970129     C     PB87F         ADD       PI87          PI87T             7 0
160300970129     C     PI87T         DIV(H)    SKA456        $XX
160400970129     C     $XX           MULT      100           PC87T             5 2
160500950622     C                   ENDIF
160600950622     C*
160700970129     C                   CLEAR                   PER34T
160800950622     C     PI87          IFNE      0
160900950622     C                   MOVE      '%'           PER34             1
161000970129     C                   MOVE      '%'           PER34T            1
161100950622     C                   ELSE
161200950622     C                   MOVE      *BLANKS       PER34
161300950622     C                   ENDIF
161400970129     C     PB87F         IFNE      0
161500970129     C                   MOVE      '%'           PER34B            1
161600970129     C                   MOVE      '%'           PER34T            1
161700970129     C                   ELSE
161800970129     C                   MOVE      *BLANKS       PER34B
161900970129     C                   ENDIF
162000930927     C**** **** **** **** **** **** **** **** ****
162100930927     C***CALCOLO TOTALI PESO
162200930927     C*TOTALE PESO FINO A 35 QUINTALI
162300090914     C     SKD(6)        ADD       SKD(7)        SKD456            8 1
162400090914     C                   ADD       SKD(8)        SKD456
162500090914     C                   ADD       SKD(9)        SKD456
162600930927     C*TOTALE PESO OLTRE  35 QUINTALI
162700090914     C     SKE(6)        ADD       SKE(7)        SKE456            8 1
162800090914     C                   ADD       SKE(8)        SKE456
162900090914     C                   ADD       SKE(9)        SKE456
163000930927     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
163100090914     C     SKF(6)        ADD       SKF(7)        SKF456            8 1
163200090914     C                   ADD       SKF(8)        SKF456
163300090914     C                   ADD       SKF(9)        SKF456
163400930927     C**** **** **** **** **** **** **** **** ****
163500970129     C***CALCOLO TOTALE VOLUME
163600930927     C*TOTALE VOLUMI
163700090914     C     SKI(6)        ADD       SKI(7)        SKI456            9 4
163800090914     C                   ADD       SKI(8)        SKI456
163900090914     C                   ADD       SKI(9)        SKI456
164000930927     C**** **** **** **** **** **** **** **** ****
164100930927     C****CALCOLO % E TOTALI SPEDIZIONI
164200970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
164300970226     C* DA PARTIRE E FUORI AFFIDABILITA'
164400970226     C     WFES          IFEQ      'F'
164500090914     C     SKA(6)        ANDEQ     *ZEROS
164600090914     C     SKA(7)        ANDEQ     *ZEROS
164700090914     C     SKA(8)        ANDEQ     *ZEROS
164800090914     C     SKA(9)        ANDEQ     *ZEROS
164900090914     C                   CLEAR                   SKM(6)
165000090914     C                   CLEAR                   SKM(7)
165100090914     C                   CLEAR                   SKM(8)
165200090914     C                   CLEAR                   SKM(9)
165300090914     C                   CLEAR                   SKN(6)
165400090914     C                   CLEAR                   SKN(7)
165500090914     C                   CLEAR                   SKN(8)
165600090914     C                   CLEAR                   SKN(9)
165700090914     C                   CLEAR                   SKP(6)
165800090914     C                   CLEAR                   SKP(7)
165900090914     C                   CLEAR                   SKP(8)
166000090914     C                   CLEAR                   SKP(9)
166100970226     C                   END
166200930927     C***CALCOLO TOTALI SPEDIZIONI
166300970129     C*TOTALE SPEDIZIONI PARTITE
166400090914     C     SKL(6)        ADD       SKL(7)        SKL456            7 0
166500090914     C                   ADD       SKL(8)        SKL456
166600090914     C                   ADD       SKL(9)        SKL456
166700970305     C*TOTALE SPEDIZIONI DA PARTIRE
166800090914     C     SKM(6)        ADD       SKM(7)        SKM456            7 0
166900090914     C                   ADD       SKM(8)        SKM456
167000090914     C                   ADD       SKM(9)        SKM456
167100930927     C**** **** **** **** **** **** **** **** ****
167200930928     C****STAMPA SINGOLA FILIALE :DETTAGLIO E TOTALE.
167300970226     C*
167400931011     C     *INOF         IFEQ      *ON
167500941012     C  N36              EXCEPT    TESTA
167600020813     C                   Z-ADD     1             XD
167700931011     C                   ENDIF
167800931011     C*
167900941012     C  N36              EXCEPT    DETFIL
168000950210     C     DS1RIS        IFNE      'R'
168100941012     C                   EXSR      WRIFIL
168200941012     C                   ENDIF
168300930928     C**** **** **** **** **** **** **** **** ****
168400930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE, IN CAMPI AREA
168500000616     C** SOLO SE DEVO STAMPARE
168600000616     C     DS1RIS        IFNE      ' '
168700930929     C                   ADD       SKA456        AKA123
168800090914     C                   ADD       SKA(6)        AKA(1)
168900090914     C                   ADD       SKA(7)        AKA(2)
169000090914     C                   ADD       SKA(8)        AKA(3)
169100090914     C                   ADD       SKA(9)        AKA(4)
169200930929     C*
169300930929     C                   ADD       PI88          ARPI88            7 0
169400970129     C                   ADD       PB88F         ARPB88            7 0
169500970130     C                   ADD       PI88T         ARP88T            7 0
169600950622     C                   ADD       PI87          ARPI87            7 0
169700970130     C                   ADD       PB87F         ARPB87            7 0
169800970130     C                   ADD       PI87T         ARP87T            7 0
169900930929     C*
170000930929     C                   ADD       SKD456        AKD123
170100090914     C                   ADD       SKD(6)        AKD(1)
170200090914     C                   ADD       SKD(7)        AKD(2)
170300090914     C                   ADD       SKD(8)        AKD(3)
170400090914     C                   ADD       SKD(9)        AKD(4)
170500930929     C*
170600930929     C                   ADD       SKE456        AKE123
170700090914     C                   ADD       SKE(6)        AKE(1)
170800090914     C                   ADD       SKE(7)        AKE(2)
170900090914     C                   ADD       SKE(8)        AKE(3)
171000090914     C                   ADD       SKE(9)        AKE(4)
171100930929     C*
171200930929     C                   ADD       SKF456        AKF123
171300090914     C                   ADD       SKF(6)        AKF(1)
171400090914     C                   ADD       SKF(7)        AKF(2)
171500090914     C                   ADD       SKF(8)        AKF(3)
171600090914     C                   ADD       SKF(9)        AKF(4)
171700930929     C*
171800931007     C                   ADD       PETS          ARPETS            9 1
171900930929     C*
172000930929     C                   ADD       SKI456        AKI123
172100090914     C                   ADD       SKI(6)        AKI(1)
172200090914     C                   ADD       SKI(7)        AKI(2)
172300090914     C                   ADD       SKI(8)        AKI(3)
172400090914     C                   ADD       SKI(9)        AKI(4)
172500970129     C*
172600930929     C                   ADD       SKL456        AKL123
172700090914     C                   ADD       SKL(6)        AKL(1)
172800090914     C                   ADD       SKL(7)        AKL(2)
172900090914     C                   ADD       SKL(8)        AKL(3)
173000090914     C                   ADD       SKL(9)        AKL(4)
173100930929     C*
173200930929     C                   ADD       SKM456        AKM123
173300090914     C                   ADD       SKM(6)        AKM(1)
173400090914     C                   ADD       SKM(7)        AKM(2)
173500090914     C                   ADD       SKM(8)        AKM(3)
173600090914     C                   ADD       SKM(9)        AKM(4)
173700930929     C*
173800090914     C                   ADD       SKN(6)        AKN(1)
173900090914     C                   ADD       SKN(7)        AKN(2)
174000090914     C                   ADD       SKN(8)        AKN(3)
174100090914     C                   ADD       SKN(9)        AKN(4)
174200000616     C                   ENDIF
174300930929     C*
174400930929     C**** **** **** **** **** **** **** **** ****
174500930929     C****PULIZIA CAMPI FILIALE (DI TOTALE E DI DETTAGLIO)
174600930929     C*CAMPI DI TOTALE
174700930929     C                   Z-ADD     0             SKA456
174800930929     C                   Z-ADD     0             PI88
174900941012     C                   Z-ADD     0             PS88
175000930929     C                   Z-ADD     0             PC88
175100970129     C                   Z-ADD     0             PB88F
175200970129     C                   Z-ADD     0             PB88
175300970129     C                   Z-ADD     0             PC88B
175400970130     C                   Z-ADD     0             PI88T
175500970129     C                   Z-ADD     0             PC88T
175600950622     C                   Z-ADD     0             PI87
175700950622     C                   Z-ADD     0             PS87
175800950622     C                   Z-ADD     0             PC87
175900970129     C                   Z-ADD     0             PB87F
176000970129     C                   Z-ADD     0             PB87
176100970129     C                   Z-ADD     0             PC87B
176200970130     C                   Z-ADD     0             PI87T
176300970129     C                   Z-ADD     0             PC87T
176400930929     C                   Z-ADD     0             SKD456
176500930929     C                   Z-ADD     0             SKE456
176600930929     C                   Z-ADD     0             SKF456
176700930929     C                   Z-ADD     0             PETS
176800930929     C                   Z-ADD     0             SKI456
176900930929     C                   Z-ADD     0             SKL456
177000930929     C                   Z-ADD     0             SKM456
177100930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
177200930929     C*PER LA STAMPA).
177300090914     C                   MOVEA     *ZEROS        SKA(6)
177400930929     C*
177500090914     C                   MOVEA     *ZEROS        SKD(6)
177600930929     C*
177700090914     C                   MOVEA     *ZEROS        SKE(6)
177800930929     C*
177900090914     C                   MOVEA     *ZEROS        SKF(6)
178000930929     C*
178100090914     C                   MOVEA     *ZEROS        SKI(6)
178200930929     C*
178300090914     C                   MOVEA     *ZEROS        SKL(6)
178400090914     C*
178500090914     C                   MOVEA     *ZEROS        SKM(6)
178600930929     C*
178700090914     C                   MOVEA     *ZEROS        SKN(6)
178800090914     C                   MOVEA     *ZEROS        SKO(6)
178900090914     C                   MOVEA     *ZEROS        SKP(6)
179000930929     C*
179100930927     C                   ENDSR
179200930929     C****************************************************************
179300930929     C* SUBROUTINE TOTCS:
179400930929     C**CALCOLO %, CALCOLO TOTALI C/SERVIZI, STAMPA DETTAGLIO C/SERVIZI,
179500930929     C*****************************************************************
179600930929     C     TOTCS         BEGSR
179700950111     C*
179800930929     C***CALCOLO TOTALI COLLI
179900930929     C*TOTALE COLLI
180000930929     C     SKA(1)        ADD       SKA(2)        SKA123            7 0
180100930929     C                   ADD       SKA(3)        SKA123
180200090914     C                   ADD       SKA(4)        SKA123
180300930929     C**** **** **** **** **** **** **** **** ****
180400970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
180500930930     C     SKA123        IFNE      0
180600970130     C* DISK B
180700970130     C     PB88CS        DIV(H)    SKA123        $XX
180800970130     C     $XX           MULT      100           PC88CB            5 2
180900970130     C* ALTRI
181000931005     C     PI88CS        DIV(H)    SKA123        $XX
181100931005     C     $XX           MULT      100           PC88CS            5 2
181200970130     C* TOTALE
181300970130     C     PB88CS        ADD       PI88CS        P88CST            7 0
181400970130     C     P88CST        DIV(H)    SKA123        $XX
181500970130     C     $XX           MULT      100           PC88CT            5 2
181600930930     C                   ENDIF
181700931005     C*
181800970130     C                   CLEAR                   PER3T
181900970130     C     PB88CS        IFNE      0
182000970130     C                   MOVE      '%'           PER3B
182100970130     C                   MOVE      '%'           PER3T
182200931005     C                   ELSE
182300970130     C                   MOVE      *BLANKS       PER3B
182400931005     C                   ENDIF
182500970130     C     PI88CS        IFNE      0
182600970130     C                   MOVE      '%'           PER3
182700970130     C                   MOVE      '%'           PER3T
182800970130     C                   ELSE
182900970130     C                   MOVE      *BLANKS       PER3
183000970130     C                   ENDIF
183100950622     C****CALCOLO % COLLI NON SPUNTATI CON PISTOLA 87 (EX 88)
183200950622     C     SKA123        IFNE      0
183300970130     C* DISK B
183400970130     C     PB87CS        DIV(H)    SKA123        $XX
183500970130     C     $XX           MULT      100           PC87CB            5 2
183600970130     C* ALTRI
183700950622     C     PI87CS        DIV(H)    SKA123        $XX
183800950622     C     $XX           MULT      100           PC87CS            5 2
183900970130     C* TOTALI
184000970130     C     PB87CS        ADD       PI87CS        P87CST            7 0
184100970203     C     P87CST        DIV(H)    SKA123        $XX
184200970130     C     $XX           MULT      100           PC87CT            5 2
184300950622     C                   ENDIF
184400950622     C*
184500970130     C                   CLEAR                   PER34T
184600970130     C     PB87CS        IFNE      0
184700970130     C                   MOVE      '%'           PER34B
184800970130     C                   MOVE      '%'           PER34T
184900950622     C                   ELSE
185000970130     C                   MOVE      *BLANKS       PER34B
185100950622     C                   ENDIF
185200970130     C     PI87CS        IFNE      0
185300970130     C                   MOVE      '%'           PER34
185400970130     C                   MOVE      '%'           PER34T
185500970130     C                   ELSE
185600970130     C                   MOVE      *BLANKS       PER34
185700970130     C                   ENDIF
185800930929     C**** **** **** **** **** **** **** **** ****
185900930929     C***CALCOLO TOTALI PESO
186000930929     C*TOTALE PESO FINO A 35 QUINTALI
186100930929     C     SKD(1)        ADD       SKD(2)        SKD123            8 1
186200930929     C                   ADD       SKD(3)        SKD123
186300090914     C                   ADD       SKD(4)        SKD123
186400930929     C*TOTALE PESO OLTRE  35 QUINTALI
186500930929     C     SKE(1)        ADD       SKE(2)        SKE123            8 1
186600930929     C                   ADD       SKE(3)        SKE123
186700090914     C                   ADD       SKE(4)        SKE123
186800930929     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
186900930929     C     SKF(1)        ADD       SKF(2)        SKF123            8 1
187000930929     C                   ADD       SKF(3)        SKF123
187100090914     C                   ADD       SKF(4)        SKF123
187200930929     C**** **** **** **** **** **** **** **** ****
187300930929     C***CALCOLO TOTALI VOLUMI
187400930929     C*TOTALE VOLUMI
187500941005     C     SKI(1)        ADD       SKI(2)        SKI123            9 4
187600930929     C                   ADD       SKI(3)        SKI123
187700090914     C                   ADD       SKI(4)        SKI123
187800930929     C**** **** **** **** **** **** **** **** ****
187900930929     C****CALCOLO % E TOTALI SPEDIZIONI
188000970130     C***CALCOLO % SPEDIZIONI FUORI AFFIDABILITA'
188100090914     C* SE GIORNO FESTIVO E NON SONO PARìITI COLLI CLEARO CAMPI DEL
188200970226     C* DA PARTIRE E FUORI AFFIDABILITA'
188300970226     C     WFES          IFEQ      'F'
188400970226     C     SKA(1)        ANDEQ     *ZEROS
188500970226     C     SKA(2)        ANDEQ     *ZEROS
188600970226     C     SKA(3)        ANDEQ     *ZEROS
188700090914     C     SKA(4)        ANDEQ     *ZEROS
188800970226     C                   CLEAR                   SKM(1)
188900970226     C                   CLEAR                   SKM(2)
189000970226     C                   CLEAR                   SKM(3)
189100090914     C                   CLEAR                   SKM(4)
189200970226     C                   CLEAR                   SKN(1)
189300970226     C                   CLEAR                   SKN(2)
189400970226     C                   CLEAR                   SKN(3)
189500090914     C                   CLEAR                   SKN(4)
189600970226     C                   CLEAR                   SKP(1)
189700970226     C                   CLEAR                   SKP(2)
189800970226     C                   CLEAR                   SKP(3)
189900090914     C                   CLEAR                   SKP(4)
190000970226     C                   END
190100930929     C*
190200930929     C***CALCOLO TOTALI SPEDIZIONI
190300970130     C*TOTALE SPEDIZIONI PARTITE
190400930929     C     SKL(1)        ADD       SKL(2)        SKL123            7 0
190500930929     C                   ADD       SKL(3)        SKL123
190600090914     C                   ADD       SKL(4)        SKL123
190700970130     C*TOTALE SPEDIZIONI DA PARTIRE
190800930929     C     SKM(1)        ADD       SKM(2)        SKM123            7 0
190900930929     C                   ADD       SKM(3)        SKM123
191000090914     C                   ADD       SKM(4)        SKM123
191100930929     C**** **** **** **** **** **** **** **** ****
191200930929     C****STAMPA FILIALE C/SERVIZI: DETTAGLIO E TOTALE.
191300931011     C     *INOF         IFEQ      *ON
191400941012     C  N36              EXCEPT    TESTA
191500020813     C                   Z-ADD     0             XD
191600931011     C                   ENDIF
191700931011     C*
191800941012     C  N36              EXCEPT    DETCS
191900930929     C**** **** **** **** **** **** **** **** ****
192000930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE C/SERVIZI, IN CAMPI AREA
192100000616     C* SOLO SE STAMPO
192200000616     C     DS1RIS        IFNE      ' '
192300930930     C                   ADD       SKA123        AKA123            7 0
192400930929     C                   ADD       SKA(1)        AKA(1)
192500930929     C                   ADD       SKA(2)        AKA(2)
192600930929     C                   ADD       SKA(3)        AKA(3)
192700090914     C                   ADD       SKA(4)        AKA(4)
192800930929     C*
192900930929     C                   ADD       PI88CS        ARPI88            7 0
193000970130     C                   ADD       PB88CS        ARPB88            7 0
193100950622     C                   ADD       PI87CS        ARPI87            7 0
193200970130     C                   ADD       PB87CS        ARPB87            7 0
193300970130     C                   ADD       P88CST        ARP88T            7 0
193400970130     C                   ADD       P87CST        ARP87T            7 0
193500930929     C*
193600930930     C                   ADD       SKD123        AKD123            8 1
193700930929     C                   ADD       SKD(1)        AKD(1)
193800930929     C                   ADD       SKD(2)        AKD(2)
193900930929     C                   ADD       SKD(3)        AKD(3)
194000090914     C                   ADD       SKD(4)        AKD(4)
194100930929     C*
194200930930     C                   ADD       SKE123        AKE123            8 1
194300930929     C                   ADD       SKE(1)        AKE(1)
194400930929     C                   ADD       SKE(2)        AKE(2)
194500930929     C                   ADD       SKE(3)        AKE(3)
194600090914     C                   ADD       SKE(4)        AKE(4)
194700930929     C*
194800930930     C                   ADD       SKF123        AKF123            8 1
194900930929     C                   ADD       SKF(1)        AKF(1)
195000930929     C                   ADD       SKF(2)        AKF(2)
195100930929     C                   ADD       SKF(3)        AKF(3)
195200090914     C                   ADD       SKF(4)        AKF(4)
195300930929     C*
195400941005     C                   ADD       SKI123        AKI123            9 4
195500930929     C                   ADD       SKI(1)        AKI(1)
195600930929     C                   ADD       SKI(2)        AKI(2)
195700930929     C                   ADD       SKI(3)        AKI(3)
195800090914     C                   ADD       SKI(4)        AKI(4)
195900930929     C*
196000930930     C                   ADD       SKL123        AKL123            7 0
196100930930     C                   ADD       SKL(1)        AKL(1)
196200930929     C                   ADD       SKL(2)        AKL(2)
196300930929     C                   ADD       SKL(3)        AKL(3)
196400090914     C                   ADD       SKL(4)        AKL(4)
196500930929     C*
196600930930     C                   ADD       SKM123        AKM123            7 0
196700930929     C                   ADD       SKM(1)        AKM(1)
196800930929     C                   ADD       SKM(2)        AKM(2)
196900930929     C                   ADD       SKM(3)        AKM(3)
197000090914     C                   ADD       SKM(4)        AKM(4)
197100930929     C*
197200930929     C                   ADD       SKN(1)        AKN(1)
197300930929     C                   ADD       SKN(2)        AKN(2)
197400930929     C                   ADD       SKN(3)        AKN(3)
197500090914     C                   ADD       SKN(4)        AKN(4)
197600000616     C                   ENDIF
197700930929     C*
197800930929     C**** **** **** **** **** **** **** **** ****
197900930929     C****PULIZIA CAMPI FILIALE C/SERVIZI (DI TOTALE E DI DETTAGLIO)
198000930929     C*CAMPI DI TOTALE
198100930929     C                   Z-ADD     0             SKA123
198200930929     C                   Z-ADD     0             PI88CS
198300970130     C                   Z-ADD     0             PB88CS
198400930929     C                   Z-ADD     0             PC88CS
198500970130     C                   Z-ADD     0             PC88CB
198600970130     C                   Z-ADD     0             P88CST
198700970130     C                   Z-ADD     0             PC88CT
198800950622     C                   Z-ADD     0             PI87CS
198900970130     C                   Z-ADD     0             PB87CS
199000950622     C                   Z-ADD     0             PC87CS
199100970130     C                   Z-ADD     0             PC87CB
199200970130     C                   Z-ADD     0             P87CST
199300970130     C                   Z-ADD     0             PC87CT
199400930929     C                   Z-ADD     0             SKD123
199500930929     C                   Z-ADD     0             SKE123
199600930929     C                   Z-ADD     0             SKF123
199700930929     C                   Z-ADD     0             SKI123
199800930929     C                   Z-ADD     0             SKL123
199900930929     C                   Z-ADD     0             SKM123
200000930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
200100930929     C*PER LA STAMPA).
200200930929     C*
200300931005     C                   MOVEA     *ZEROS        SKA(1)
200400930929     C*
200500931005     C                   MOVEA     *ZEROS        SKD(1)
200600930929     C*
200700931005     C                   MOVEA     *ZEROS        SKE(1)
200800930929     C*
200900931005     C                   MOVEA     *ZEROS        SKF(1)
201000930929     C*
201100931005     C                   MOVEA     *ZEROS        SKI(1)
201200930929     C*
201300931005     C                   MOVEA     *ZEROS        SKL(1)
201400930929     C*
201500931005     C                   MOVEA     *ZEROS        SKM(1)
201600930929     C*
201700931005     C                   MOVEA     *ZEROS        SKN(1)
201800970206     C                   MOVEA     *ZEROS        SKO(1)
201900970206     C                   MOVEA     *ZEROS        SKP(1)
202000930929     C*
202100930929     C                   ENDSR
202200930929     C****************************************************************
202300930929     C* SUBROUTINE TOTARE:
202400930929     C**CALCOLO %, CALCOLO TOTALI AREA, STAMPA DETTAGLIO AREA,
202500930929     C*****************************************************************
202600930929     C     TOTARE        BEGSR
202700970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
202800930930     C     AKA123        IFNE      0
202900970130     C* DISK B
203000970130     C     ARPB88        DIV(H)    AKA123        $XX
203100970130     C     $XX           MULT      100           APC88B            5 2
203200970130     C* ALTRI
203300970130     C     ARPI88        DIV(H)    AKA123        $XX
203400970130     C     $XX           MULT      100           ARPC88            5 2
203500970130     C* TOTALE
203600970130     C     ARP88T        DIV(H)    AKA123        $XX
203700970130     C     $XX           MULT      100           APC88T            5 2
203800930930     C                   ENDIF
203900931005     C*
204000970130     C                   CLEAR                   PER3T
204100970130     C     ARPB88        IFNE      0
204200970130     C                   MOVE      '%'           PER3B
204300970130     C                   MOVE      '%'           PER3T
204400931005     C                   ELSE
204500970130     C                   MOVE      *BLANKS       PER3B
204600931005     C                   ENDIF
204700970130     C     ARPI88        IFNE      0
204800970130     C                   MOVE      '%'           PER3
204900970130     C                   MOVE      '%'           PER3T
205000970130     C                   ELSE
205100970130     C                   MOVE      *BLANKS       PER3
205200970130     C                   ENDIF
205300950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
205400950622     C     AKA123        IFNE      0
205500970130     C* DISK B
205600970130     C     ARPB87        DIV(H)    AKA123        $XX
205700970130     C     $XX           MULT      100           APC87B            5 2
205800970130     C* ALTRI
205900950622     C     ARPI87        DIV(H)    AKA123        $XX
206000950622     C     $XX           MULT      100           ARPC87            5 2
206100970130     C* TOTALE
206200970130     C     ARP87T        DIV(H)    AKA123        $XX
206300970130     C     $XX           MULT      100           APC87T            5 2
206400950622     C                   ENDIF
206500950622     C*
206600970130     C                   CLEAR                   PER34T
206700970130     C     ARPB87        IFNE      0
206800970130     C                   MOVE      '%'           PER34B
206900970130     C                   MOVE      '%'           PER34T
207000950622     C                   ELSE
207100970130     C                   MOVE      *BLANKS       PER34B
207200950622     C                   ENDIF
207300970130     C     ARPI87        IFNE      0
207400970130     C                   MOVE      '%'           PER34
207500970130     C                   MOVE      '%'           PER34T
207600970130     C                   ELSE
207700970130     C                   MOVE      *BLANKS       PER34
207800970130     C                   ENDIF
207900930929     C**** **** **** **** **** **** **** **** ****
208000930929     C****STAMPA AREA :DETTAGLIO E TOTALE.
208100020813     C     XD            IFLE      7
208200941012     C  N36              EXCEPT    DETARE
208300931011     C                   ELSE
208400941012     C  N36              EXCEPT    TESTA
208500941012     C  N36              EXCEPT    DETARE
208600931011     C                   ENDIF
208700930929     C**** **** **** **** **** **** **** **** ****
208800931007     C****SOMMA CAMPI TOTALI AREA, IN CAMPI TOT. GENERALE
208900000616     C* SOLO SE  STAMPO
209000000616     C     DS1RIS        IFNE      ' '
209100931006     C*
209200931008     C                   ADD       AKA123        TKA123            7 0
209300931006     C                   ADD       AKA(1)        TKA(1)
209400931006     C                   ADD       AKA(2)        TKA(2)
209500931006     C                   ADD       AKA(3)        TKA(3)
209600090914     C                   ADD       AKA(4)        TKA(4)
209700931006     C*
209800970130     C                   ADD       ARP88T        TGP88T            7 0
209900970130     C                   ADD       ARP87T        TGP87T            7 0
210000931006     C*
210100931008     C                   ADD       AKD123        TKD123            8 1
210200931006     C                   ADD       AKD(1)        TKD(1)
210300931006     C                   ADD       AKD(2)        TKD(2)
210400931006     C                   ADD       AKD(3)        TKD(3)
210500090914     C                   ADD       AKD(4)        TKD(4)
210600931006     C*
210700931008     C                   ADD       AKE123        TKE123            8 1
210800931006     C                   ADD       AKE(1)        TKE(1)
210900931006     C                   ADD       AKE(2)        TKE(2)
211000931006     C                   ADD       AKE(3)        TKE(3)
211100090914     C                   ADD       AKE(4)        TKE(4)
211200931006     C*
211300931008     C                   ADD       AKF123        TKF123            8 1
211400931006     C                   ADD       AKF(1)        TKF(1)
211500931006     C                   ADD       AKF(2)        TKF(2)
211600931006     C                   ADD       AKF(3)        TKF(3)
211700090914     C                   ADD       AKF(4)        TKF(4)
211800931006     C*
211900931007     C                   ADD       ARPETS        TGPETS            9 1
212000931006     C*
212100941005     C                   ADD       AKI123        TKI123            9 4
212200931006     C                   ADD       AKI(1)        TKI(1)
212300931006     C                   ADD       AKI(2)        TKI(2)
212400931006     C                   ADD       AKI(3)        TKI(3)
212500090914     C                   ADD       AKI(4)        TKI(4)
212600931006     C*
212700931008     C                   ADD       AKL123        TKL123            7 0
212800931006     C                   ADD       AKL(1)        TKL(1)
212900931006     C                   ADD       AKL(2)        TKL(2)
213000931006     C                   ADD       AKL(3)        TKL(3)
213100090914     C                   ADD       AKL(4)        TKL(4)
213200000616     C                   ENDIF
213300931006     C*
213400930930     C**** **** **** **** **** **** **** **** ****
213500930930     C****PULIZIA CAMPI AREA    (DI TOTALE E DI DETTAGLIO)
213600930929     C*CAMPI DI TOTALE
213700930930     C                   Z-ADD     0             AKA123
213800930930     C                   Z-ADD     0             ARPI88
213900970130     C                   Z-ADD     0             ARPB88
214000970130     C                   Z-ADD     0             APC88B
214100970130     C                   Z-ADD     0             ARPC88
214200970130     C                   Z-ADD     0             ARP88T
214300970130     C                   Z-ADD     0             APC88T
214400950622     C                   Z-ADD     0             ARPI87
214500970130     C                   Z-ADD     0             ARPB87
214600950622     C                   Z-ADD     0             ARPC87
214700970130     C                   Z-ADD     0             APC87B
214800970130     C                   Z-ADD     0             ARP87T
214900970130     C                   Z-ADD     0             APC87T
215000930930     C                   Z-ADD     0             AKD123
215100930930     C                   Z-ADD     0             AKE123
215200930930     C                   Z-ADD     0             AKF123
215300930930     C                   Z-ADD     0             ARPETS
215400930930     C                   Z-ADD     0             AKI123
215500930930     C                   Z-ADD     0             AKL123
215600930930     C                   Z-ADD     0             AKM123
215700930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
215800930929     C*PER LA STAMPA).
215900931005     C                   MOVEA     *ZEROS        AKA(1)
216000930929     C*
216100931005     C                   MOVEA     *ZEROS        AKD(1)
216200930929     C*
216300931005     C                   MOVEA     *ZEROS        AKE(1)
216400930929     C*
216500931005     C                   MOVEA     *ZEROS        AKF(1)
216600930929     C*
216700931005     C                   MOVEA     *ZEROS        AKI(1)
216800930929     C*
216900931005     C                   MOVEA     *ZEROS        AKL(1)
217000930929     C*
217100931005     C                   MOVEA     *ZEROS        AKM(1)
217200930929     C*
217300931005     C                   MOVEA     *ZEROS        AKN(1)
217400930929     C*
217500930929     C                   ENDSR
217600930929     C****************************************************************
217700931006     C* SUBROUTINE ELATRA: ELABORAZIONE TRANSITI
217800931006     C*****************************************************************
217900931006     C     ELATRA        BEGSR
218000950111     C*
218100931008     C                   Z-ADD     0             FLDTXT
218200950111     C*
218300950111     C* ?    C O L L I    T R A N S I T O    ?
218400950329     C*
218500931007     C*POSIZ.PER'DATA DA'(DATA USCITA DAL TRANSITO).
218600020823     C     KDTA          SETLL     FNBTT38L
218700020823     C     KDTA          READE     FNBTT38L                               31
218800931006     C* CICLO LETTURA COLLI
218900970130    1C     *IN31         DOWEQ     *OFF
219000931007     C*
219100931013     C* ENTRA SOLO AL CAMBIO BOLLA.
219200970130    2C     DSTXT1        IFNE      FLDTXT
219300950111     C                   Z-ADD     DSTXT1        FLDTXT
219400020823     C     KBTP11        CHAIN     FNBTP11L                           32
219500931130      *
219600931130      * SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
219700970130    3C     *IN32         IFEQ      *OFF
219800950111     C*
219900950111     C                   Z-ADD     1             WA
220000950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
220100950111     C   32              MOVEL     D3A(WA)       DS3A
220200950111     C  N32              CLEAR                   DS3A
220300931130      *
220400950111     C* REPERIM.DATI BOLLA, (CORR.,ESPR.,RAPID,NON DIFFERENZIANDO I C/SERV.)
220500931007     C* QUINDI SOLO PER BOLLE-TRANSITO.
220600931007     C                   EXSR      DTBOT
220700950111     C* REPERIMENTO DATI BOLLA, PER CALCOLO TRANSITI
220800950111     C                   EXSR      DATBOT
220900931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN RECUPERO PER FARLA SCARTARE
221000970130   X3C                   ELSE
221100931006     C                   MOVEL     'R'           §3ARBL
221200970130    3C                   ENDIF
221300970130    2C                   ENDIF
221400931006     C*
221500931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
221600970130    2C     §3ARBL        IFNE      'R'
221700931013     C* CALCOLI-TRANSITI
221800931007     C                   EXSR      CALCTR
221900970130    2C                   ENDIF
222000931006     C*
222100020823     C     KDTA          READE     FNBTT38L                               31
222200970130    1C                   ENDDO
222300970505     C* AGGIUNGO I COLLI IN DISGUIDO
222400970505     C                   ADD       WDIS88        TRPI88
222500000616     C                   CLEAR                   WDIS88
222600931006     C***
222700950111     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
222800950111     C* ?    S P E D I Z I O N I    T R A N S I T O    ?
222900931007     C*
223000950111     C* POSIZIONAMENTO PER DATA INIZIO PERIODO
223100020823     C     KDTA          SETLL     FNBTP15L
223200020823     C     KDTA          READE     FNBTP15L                               31
223300931013     C*
223400970130    1C     *IN31         DOWEQ     *OFF
223500931007     C*
223600931130      * TEST SE E' RECUPERO
223700950111     C                   Z-ADD     1             WA
223800950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
223900950111     C   32              MOVEL     D3A(WA)       DS3A
224000950111     C  N32              CLEAR                   DS3A
224100931006     C*
224200931006     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
224300970130    2C     §3ARBL        IFNE      'R'
224400931007     C                   EXSR      CSPETR
224500970130    2C                   ENDIF
224600931006     C*
224700950111     C*
224800931006     C*LETTURA DENTRO AL CICLO
224900020823     C     KDTA          READE     FNBTP15L                               31
225000970130    1C                   ENDDO
225100931006     C*
225200931006     C                   ENDSR
225300931007     C*****************************************************************
225400931007     C* SUBROUTINE DTBOT: NON DIFFERENZIA TRA FILIALE E C/SERVIZI
225500931007     C*****************************************************************
225600931007     C     DTBOT         BEGSR
225700950111     C*
225800931007     C*TEST SE E' C/SERVIZI O FILIALE
225900931007     C*MI SERVE SOLO PER SAPERE SE ADDIZIONARE O MENO IL PESO TASSABILE.
226000931007     C     §3ARBL        IFEQ      'C'
226100931007     C                   SETON                                        04
226200931007     C                   ELSE
226300931007     C                   SETOFF                                       04
226400931007     C                   ENDIF
226500931007     C*
226600931007     C*FISSO GLI INDICI DI SCHIERA....
226700931007     C*
226800090914     C* SE TIPO SPEDIZIONE E' h 10 30
226900090914     c                   select
227000090914     C     BTPTSP        wheneq    'H'
227100090914     C                   Z-ADD     4             X
227200090914     c
227300090914     C* SE TIPO SPEDIZIONE E' priority
227400090914     C     BTPTSP        wheneq    'E'
227500931007     C                   Z-ADD     2             X
227600931007     C*
227700950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
227800090914     C     BTPTSP        wheneq    'D'
227900931007     C                   Z-ADD     3             X
228000090914     C                   other
228100950111     C                   Z-ADD     1             X
228200090914     C                   ENDSL
228300931007     C*
228400931007     C                   ENDSR
228500950111     C************************************************************
228600950111     C* SUBROUTINE DATBOT: REPERIMENTO DATI BOLLA PER CALCOLI (TRANSITI)
228700950111     C*****************************************************************
228800950111     C     DATBOT        BEGSR
228900021213     ***C*
229000021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
229100021213     ***C     BTPPKB        DIV(H)    BTPNCL        PESCOL            8 1
229200021213     *** *
229300021213     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
229400021213     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
229500021213     *** * BTPVLC=Volume rilevato)
229600050111     ***C     BTPNCR        IFEQ      BTPNCL
229700021213     ***C                   Z-ADD     BTPVLC        BTPVLB
229800021213     ***C                   ELSE
229900021213     ***C     BTPVLC        IFGE      BTPVLB
230000021213     ***C                   Z-ADD     BTPVLC        BTPVLB
230100021213     ***C                   ENDIF
230200021213     ***C                   ENDIF
230300021213     ***C*
230400021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
230500021213     ***C     BTPVLB        DIV(H)    BTPNCL        VOLUM            12 4
230600021220      *
230700021220      * Verifico i dati misurati con la macchina smistacolli
230800021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
230900021220      * e calcolo il VOLUME unitario medio per collo da usare se non
231000021220      * tutti i colli sono stati volumati dalla smistacolli
231100021220      *
231200030213      **** Verifico se sono stati volumati tutti i colli
231300030213     c***                clear                   Wsivol
231400021220     c                   clear                   Wvolu
231500050111     c                   IF        BTPncr = BTPncl
231600021220     c                   z-add     BTPvlc        Wvolu
231700030213     c***                eval      Wsivol =  'S'
231800021220   x1c                   else
231900021220if  2c                   if        BTPvlc >= BTPvlb
232000021220     c                   z-add     BTPvlc        Wvolu
232100021220   x2c                   else
232200021220     c                   z-add     BTPvlb        Wvolu
232300021220e   2c                   endif
232400021220e   1c                   endif
232500021220      *
232600021220      * Calcolo il volume medio
232700021220     c     Wvolu         div(H)    BTPncl        wVOLUM
232800021220      *
232900021220      * Verifico i dati misurati con la macchina smistacolli
233000021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
233100021220      * e calcolo il PESO   unitario medio per collo da usare se non
233200021220      * tutti i colli sono stati pesati   dalla smistacolli
233300021220      *
233400030213      **** Verifico se sono stati pesati   tutti i colli
233500030213     c***                clear                   Wsipes
233600021220     c                   clear                   Wpeso
233700050111     c                   If        BTPncp = BTPncl
233800021220     c                   z-add     BTPpkc        Wpeso
233900030213     c***                eval      Wsipes =  'S'
234000021220   x1c                   else
234100021220if  2c                   if        BTPpkc >= BTPpkb
234200021220     c                   z-add     BTPpkc        Wpeso
234300021220   x2c                   else
234400021220     c                   z-add     BTPpkb        Wpeso
234500021220e   2c                   endif
234600021220e   1c                   endif
234700021220      *
234800021220      * Calcolo il peso medio
234900021220     c     Wpeso         div(H)    BTPncl        wPESCOL
235000950111     C*
235100950111     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
235200021213     ***C     BTPPKB        IFLE      3500
235300021213     C     Wpeso         IFLE      3500
235400950111     C                   SETON                                        06
235500950111     C                   ELSE
235600950111     C                   SETOFF                                       06
235700950111     C                   ENDIF
235800950111     C*
235900950111     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
236000021213     ***C                   Z-ADD     BTPPKB        PESTAX            8 0
236100021213     C                   z-add     Wpeso         PESTAX
236200950111     C*
236300950111     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
236400021213     ***C     BTPVLB        MULT      §QTKPM        COMPKT
236500021213     C     Wvolu         MULT      §QTKPM        COMPKT
236600950111     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
236700950111     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
236800950111     C     COMPKT        IFGT      PESTAX
236900950111     C                   Z-ADD     COMPKT        PESTAX
237000950111     C                   END
237100950111     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
237200950111     C                   Z-ADD     0             COMPKT            8 0
237300950111     C     PESTAX        DIV       §3IARR        COMPKT
237400950111     C                   MVR                     RESTO             3 0
237500950111     C     RESTO         IFGT      0
237600950111     C     COMPKT        MULT      §3IARR        PESTAX
237700950111     C                   ADD       §3IARR        PESTAX
237800950111     C                   ENDIF
237900950111     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
238000021213     C     PESTAX        DIV(H)    BTPNCL        PESTAS
238100950111     C*
238200950111     C                   ENDSR
238300931007     C****************************************************************
238400931007     C* SUBROUTINE CALCTR:
238500931007     C* SOMMA VALORI COLLO, IN TRANSITI.
238600931007     C* COLLI;COLLI NON SPUNT;PESO;PESO TASSABILE;VOLUME.
238700931007     C*****************************************************************
238800931007     C     CALCTR        BEGSR
238900950111     C*
239000931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
239100931007     C******COLLI
239200931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
239300931007     C                   ADD       1             XKA(X)
239400931007     C*
239500931007     C******COLLI NON SPUNTATI IN PARTENZA: 88.
239600950111     C     BTTPUT        IFEQ      88
239700950331     C     BTTPUT        OREQ      89
239800970430     C     BTTPUT        OREQ      98
239900970430     C     BTTPUT        OREQ      96
240000941012     C  N04              ADD       1             TRPI88            7 0
240100931007     C                   ENDIF
240200950622     C******COLLI PISTOLA 87 (EX 88 E 89)
240300950622     C     BTTPUT        IFEQ      87
240400950622     C  N04              ADD       1             TRPI87            7 0
240500950622     C                   ENDIF
240600931007     C*
240700931007     C******PESO
240800931007     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
240900030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
241000030213      * Peso medio
241100030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
241200030213if  1c***                if        Wsipes = 'S'
241300030213     c***                z-add(H)  BTTpuc        PESCOL
241400030213   x1c***                else
241500021220     c                   z-add     Wpescol       PESCOL
241600030213e   1c***                endif
241700950403     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
241800931007     C   06              ADD       PESCOL        XKD(X)
241900931007     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
242000931007     C  N06              ADD       PESCOL        XKE(X)
242100931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
242200931007     C                   ADD       PESCOL        XKF(X)
242300931007     C*
242400931007     C*
242500931007     C******PESO TASSABILE
242600931007     C  N04
242700931007     CAN 06              ADD       PESTAS        TRPETS            9 1
242800931007     C*
242900931007     C*
243000931007     C******VOLUME
243100021213     ***C     BTTVUC        IFGT      0
243200021213     ***C                   Z-ADD(H)  BTTVUC        WVOLUM
243300021213     ***C                   ELSE
243400021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
243500021213     ***C                   ENDIF
243600021220     ***C*
243700021220     ***C                   ADD       WVOLUM        XKI(X)
243800030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
243900030213      * Volume medio
244000030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
244100030213if  1c***                if        Wsivol = 'S'
244200030213     c***                z-add     BTTvuc        Volum
244300030213   x1c***                else
244400021220     c                   z-add     Wvolum        Volum
244500030213e   1c***                endif
244600021220     c                   add       VOLUM         XKI(X)
244700931007     C*
244800931007     C                   ENDSR
244900930929     C****************************************************************
245000931007     C* SUBROUTINE CSPETR:  CALCOLO SPEDIZIONI -TRANSITI
245100931007     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
245200931007     C*****************************************************************
245300931007     C     CSPETR        BEGSR
245400931007     C*
245500931007     C*FISSO GLI INDICI DI SCHIERA....
245600090914     c                   exsr      DTBOT
245700931007     C*
245800931007     C*
245900931007     C******SPEDIZIONI
246000931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
246100931007     C* SOMMO SOLO IN SCHIERA DEL TOTALE
246200931007     C                   ADD       1             XKL(X)
246300931007     C*
246400931007     C                   ENDSR
246500931007     C****************************************************************
246600931007     C* SUBROUTINE TOTTRA:
246700931007     C**CALCOLO TOTALI TRANSITI STAMPA DETTAGLIO TRANSITI
246800931007     C*****************************************************************
246900931007     C     TOTTRA        BEGSR
247000950111     C*
247100931007     C**** **** **** **** **** **** **** **** ****
247200970218     C* AGGIUNGO I COLLI IN DISGUIDO AL TOTALE COLLI CORRIERE
247300970218     C                   ADD       WDIS          XKA(1)
247400000616     C                   CLEAR                   WDIS
247500931007     C****CALCOLO TOTALE COLLI:
247600931007     C     XKA(1)        ADD       XKA(2)        XKA123            7 0
247700931008     C                   ADD       XKA(3)        XKA123
247800090914     C                   ADD       XKA(4)        XKA123
247900950331     C*
248000931007     C**** **** **** **** **** **** **** **** ****
248100931007     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
248200931007     C     XKA123        IFNE      0
248300931007     C     TRPI88        DIV(H)    XKA123        $XX
248400931007     C     $XX           MULT      100           TRPC88            5 2
248500931007     C                   ENDIF
248600931007     C*
248700931008     C     TRPI88        IFNE      0
248800931007     C                   MOVE      '%'           PER3              1
248900931007     C                   ELSE
249000931007     C                   MOVE      *BLANKS       PER3
249100931007     C                   ENDIF
249200950622     C****CALCOLO % COLLI PISTOLA 87 (EX 88 E 89)
249300950622     C     XKA123        IFNE      0
249400950622     C     TRPI87        DIV(H)    XKA123        $XX
249500950622     C     $XX           MULT      100           TRPC87            5 2
249600950622     C                   ENDIF
249700950622     C*
249800950622     C     TRPI87        IFNE      0
249900950622     C                   MOVE      '%'           PER34             1
250000950622     C                   ELSE
250100950622     C                   MOVE      *BLANKS       PER34
250200950622     C                   ENDIF
250300931007     C**** **** **** **** **** **** **** **** ****
250400931007     C****CALCOLO TOTALI PESO:
250500931007     C*
250600931007     C*TOTALE PESO FINO A 35 QUINTALI
250700931007     C     XKD(1)        ADD       XKD(2)        XKD123            8 1
250800931007     C                   ADD       XKD(3)        XKD123
250900090914     C                   ADD       XKD(4)        XKD123
251000931007     C*TOTALE PESO OLTRE  35 QUINTALI
251100931007     C     XKE(1)        ADD       XKE(2)        XKE123            8 1
251200931007     C                   ADD       XKE(3)        XKE123
251300090914     C                   ADD       XKE(4)        XKE123
251400931007     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
251500931007     C     XKF(1)        ADD       XKF(2)        XKF123            8 1
251600931007     C                   ADD       XKF(3)        XKF123
251700090914     C                   ADD       XKF(4)        XKF123
251800931007     C**** **** **** **** **** **** **** **** ****
251900931007     C****CALCOLO TOTALE VOLUME
252000941005     C     XKI(1)        ADD       XKI(2)        XKI123            9 4
252100931007     C                   ADD       XKI(3)        XKI123
252200090914     C                   ADD       XKI(4)        XKI123
252300931007     C**** **** **** **** **** **** **** **** ****
252400931007     C****CALCOLO TOTALE SPEDIZIONI
252500931007     C     XKL(1)        ADD       XKL(2)        XKL123            7 0
252600931007     C                   ADD       XKL(3)        XKL123
252700090914     C                   ADD       XKL(4)        XKL123
252800931007     C**** **** **** **** **** **** **** **** ****
252900931007     C****STAMPA TRANSITI :DETTAGLIO E TOTALE.
253000941012     C  N36              EXCEPT    DETTRA
253100950210     C     DS1RIS        IFNE      'R'
253200941012     C                   EXSR      WRITRA
253300941012     C                   ENDIF
253400931007     C**** **** **** **** **** **** **** **** ****
253500931007     C****SOMMA CAMPI TOTALI TRANSITI IN CAMPI TOTALI TOT.GEN.
253600000616     C**  SOLO SE NON STAMPO
253700000616     C     DS1RIS        IFNE      ' '
253800931007     C                   ADD       XKA123        TKA123
253900931007     C                   ADD       XKA(1)        TKA(1)
254000931007     C                   ADD       XKA(2)        TKA(2)
254100931007     C                   ADD       XKA(3)        TKA(3)
254200090914     C                   ADD       XKA(4)        TKA(4)
254300931007     C*
254400970130     C                   ADD       TRPI88        TGP88T            7 0
254500970130     C                   ADD       TRPI87        TGP87T            7 0
254600931007     C*
254700931007     C                   ADD       XKD123        TKD123
254800931007     C                   ADD       XKD(1)        TKD(1)
254900931007     C                   ADD       XKD(2)        TKD(2)
255000931007     C                   ADD       XKD(3)        TKD(3)
255100090914     C                   ADD       XKD(4)        TKD(4)
255200931007     C*
255300931007     C                   ADD       XKE123        TKE123
255400931007     C                   ADD       XKE(1)        TKE(1)
255500931007     C                   ADD       XKE(2)        TKE(2)
255600931007     C                   ADD       XKE(3)        TKE(3)
255700090914     C                   ADD       XKE(4)        TKE(4)
255800931007     C*
255900931007     C                   ADD       XKF123        TKF123
256000931007     C                   ADD       XKF(1)        TKF(1)
256100931007     C                   ADD       XKF(2)        TKF(2)
256200931007     C                   ADD       XKF(3)        TKF(3)
256300090914     C                   ADD       XKF(4)        TKF(4)
256400931007     C*
256500931007     C                   ADD       TRPETS        TGPETS            9 1
256600931007     C*
256700931007     C                   ADD       XKI123        TKI123
256800931007     C                   ADD       XKI(1)        TKI(1)
256900931007     C                   ADD       XKI(2)        TKI(2)
257000931007     C                   ADD       XKI(3)        TKI(3)
257100090914     C                   ADD       XKI(4)        TKI(4)
257200931007     C*
257300931007     C                   ADD       XKL123        TKL123
257400931007     C                   ADD       XKL(1)        TKL(1)
257500931007     C                   ADD       XKL(2)        TKL(2)
257600931007     C                   ADD       XKL(3)        TKL(3)
257700090914     C                   ADD       XKL(4)        TKL(4)
257800000616     C                   ENDIF
257900931007     C*
258000931007     C**** **** **** **** **** **** **** **** ****
258100931008     C****PULIZIA CAMPI TRANSITO (DI TOTALE E DI DETTAGLIO)
258200931007     C*CAMPI DI TOTALE
258300931007     C                   Z-ADD     0             XKA123
258400931007     C                   Z-ADD     0             TRPI88
258500931007     C                   Z-ADD     0             TRPC88
258600950622     C                   Z-ADD     0             TRPI87
258700950622     C                   Z-ADD     0             TRPC87
258800931007     C                   Z-ADD     0             XKD123
258900931007     C                   Z-ADD     0             XKE123
259000931007     C                   Z-ADD     0             XKF123
259100931007     C                   Z-ADD     0             TRPETS
259200931007     C                   Z-ADD     0             XKI123
259300931007     C                   Z-ADD     0             XKL123
259400931007     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
259500931007     C*PER LA STAMPA).
259600931007     C                   MOVEA     *ZEROS        XKA(1)
259700931007     C*
259800931007     C                   MOVEA     *ZEROS        XKD(1)
259900931007     C*
260000931007     C                   MOVEA     *ZEROS        XKE(1)
260100931007     C*
260200931007     C                   MOVEA     *ZEROS        XKF(1)
260300931007     C*
260400931007     C                   MOVEA     *ZEROS        XKI(1)
260500931007     C*
260600931007     C                   MOVEA     *ZEROS        XKL(1)
260700931007     C*
260800931007     C                   ENDSR
260900931007     C****************************************************************
261000931008     C* SUBROUTINE TOTGEN:
261100931008     C**CALCOLO TOTALI TOT.GEN. STAMPA DETTAGLIO TOT.GEN.
261200931008     C*****************************************************************
261300931008     C     TOTGEN        BEGSR
261400950111     C*
261500931008     C**** **** **** **** **** **** **** **** ****
261600931008     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
261700931008     C     TKA123        IFNE      0
261800970130     C* TOTALE
261900970131     C     TGP88T        DIV(H)    TKA123        $XX
262000970131     C     $XX           MULT      100           TGC88T            5 2
262100931008     C                   ENDIF
262200931008     C*
262300970131     C     TGP88T        IFNE      0
262400970131     C                   MOVE      '%'           PER3              1
262500970131     C                   ELSE
262600970131     C                   MOVE      *BLANKS       PER3
262700970131     C                   ENDIF
262800950622     C****CALCOLO % COLLI CON PISTOLA 87 (EX 88 E 89)
262900950622     C     TKA123        IFNE      0
263000970130     C* TOTALE
263100970131     C     TGP87T        DIV(H)    TKA123        $XX
263200970131     C     $XX           MULT      100           TGC87T            5 2
263300950622     C                   ENDIF
263400950622     C*
263500970131     C     TGP87T        IFNE      0
263600950622     C                   MOVE      '%'           PER34             1
263700950622     C                   ELSE
263800950622     C                   MOVE      *BLANKS       PER34
263900950622     C                   ENDIF
264000931008     C**** **** **** **** **** **** **** **** ****
264100931008     C****STAMPA TOT.GEN. :DETTAGLIO E TOTALE.
264200941012     C  N36              EXCEPT    DETTOT
264300931008     C**** **** **** **** **** **** **** **** ****
264400931008     C****PULIZIA CAMPI TOT.GEN.(DI TOTALE E DI DETTAGLIO)
264500931008     C*CAMPI DI TOTALE
264600931008     C                   Z-ADD     0             TKA123
264700970131     C                   Z-ADD     0             TGP88T
264800970131     C                   Z-ADD     0             TGP87T
264900970131     C                   Z-ADD     0             TGC88T
265000970131     C                   Z-ADD     0             TGC87T
265100931008     C                   Z-ADD     0             TKD123
265200931008     C                   Z-ADD     0             TKE123
265300931008     C                   Z-ADD     0             TKF123
265400931008     C                   Z-ADD     0             TGPETS
265500931008     C                   Z-ADD     0             TKI123
265600931008     C                   Z-ADD     0             TKL123
265700931008     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
265800931008     C*PER LA STAMPA).
265900931008     C                   MOVEA     *ZEROS        TKA(1)
266000931008     C                   MOVEA     *ZEROS        TKD(1)
266100931008     C                   MOVEA     *ZEROS        TKE(1)
266200931008     C                   MOVEA     *ZEROS        TKF(1)
266300931008     C                   MOVEA     *ZEROS        TKI(1)
266400931008     C                   MOVEA     *ZEROS        TKL(1)
266500940126      *
266600941012      * Verifico se vi sono fogli viaggio aperti
266700941012     C                   EXSR      FOGVIA
266800931130      *
266900931008     C                   ENDSR
267000941012      ****************************************************************
267100941012      * Scrivo in archivio il riepilogo per filiale (totali e c/serv.)
267200941012      ****************************************************************
267300941012     C     WRIFIL        BEGSR
267400941012      *
267500941012      * ---------------------- TOTALI DI FILIALE -------------------------------
267600941012      *
267700941012     C                   Z-ADD     SIMFEL        SPAFLE
267800941012     C                   Z-ADD     COMFIL        SPALNP
267900950210     C                   Z-ADD     DS1ADA        SPADRE
268000941012     C                   MOVE      *BLANK        SPATRE
268100941012      * ---- COLLI ----
268200090914     C                   Z-ADD     SKA(6)        SPACPC
268300090914     C                   Z-ADD     SKA(7)        SPACPE
268400090914     C                   Z-ADD     SKA(8)        SPACPD
268500090914     C                   Z-ADD     SKA(9)        SPACPh
268600970129     C                   Z-ADD     PB88F         SPA88B
268700970129     C                   Z-ADD     PI88          SPA88A
268800970129     C                   Z-ADD     PB87F         SPA87B
268900970129     C                   Z-ADD     PI87          SPA87A
269000941012      * ---- PESI ----
269100090914     C                   Z-ADD     SKD(6)        SPAPFC
269200090914     C                   Z-ADD     SKD(7)        SPAPFE
269300090914     C                   Z-ADD     SKD(8)        SPAPFD
269400090914     C                   Z-ADD     SKD(9)        SPAPFh
269500090918     C                   Z-ADD     SKE(6)        SPAPOC
269600090918     C                   Z-ADD     SKE(7)        SPAPOE
269700090914     C                   Z-ADD     SKE(8)        SPAPOD
269800090914     C                   Z-ADD     SKE(9)        SPAPOh
269900941012     C                   Z-ADD     PETS          SPAPTA
270000941012      * ---- VOLUMI ----
270100090915     C                   Z-ADD     SKI(6)        SPAVPC
270200090915     C                   Z-ADD     SKI(7)        SPAVPE
270300090914     C                   Z-ADD     SKI(8)        SPAVPD
270400090914     C                   Z-ADD     SKI(9)        SPAVPh
270500941012      * ---- SPEDIZIONI --
270600090915     C                   Z-ADD     SKL(6)        SPASPC
270700090915     C                   Z-ADD     SKL(7)        SPASPE
270800090914     C                   Z-ADD     SKL(8)        SPASPD
270900090914     C                   Z-ADD     SKL(9)        SPASPh
271000090915     C                   Z-ADD     SKM(6)        SPASDC
271100090915     C                   Z-ADD     SKM(7)        SPASDE
271200090914     C                   Z-ADD     SKM(8)        SPASDD
271300090914     C                   Z-ADD     SKM(9)        SPASDh
271400090915     C                   Z-ADD     SKO(6)        SPASFC
271500090915     C                   Z-ADD     SKO(7)        SPASFE
271600090914     C                   Z-ADD     SKO(8)        SPASFD
271700090914     C                   Z-ADD     SKO(9)        SPASFh
271800090915     C                   Z-ADD     SKP(6)        SPATFC
271900090915     C                   Z-ADD     SKP(7)        SPATFE
272000090914     C                   Z-ADD     SKP(8)        SPATFD
272100090914     C                   Z-ADD     SKP(9)        SPATFh
272200941012      *
272300040921     c                   move      *blanks       spaftr
272400040921     c                   z-add     dateu         spadtr
272500970204     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI SCRIVO RECORD CON
272600970226     C* TIPO = 'F'
272700970226     C     WFES          IFEQ      'F'
272800970226     C     SPACPE        ANDEQ     *ZEROS
272900970204     C     SPACPC        ANDEQ     *ZEROS
273000970204     C     SPACPD        ANDEQ     *ZEROS
273100970204     C                   MOVE      'F'           SPATRE
273200970204     C                   END
273300090911     C                   WRITE     FISPA000
273400941012      *
273500941012      * --------------------- CONTO SERVIZI DI FILIALE -------------------------
273600941012      *
273700941012      * Finche sono qua (totali di filiale), scrivo anche i c/serv. di filiale
273800941012     C                   Z-ADD     SIMFEL        SPAFLE
273900941012     C                   Z-ADD     COMFIL        SPALNP
274000950210     C                   Z-ADD     DS1ADA        SPADRE
274100941012     C                   MOVE      'C'           SPATRE
274200941012      * ---- COLLI ----
274300090915     C                   Z-ADD     SKA(11)       SPACPC
274400090915     C                   Z-ADD     SKA(12)       SPACPE
274500090914     C                   Z-ADD     SKA(13)       SPACPD
274600090914     C                   Z-ADD     SKA(14)       SPACPh
274700970130     C                   Z-ADD     PB88          SPA88B
274800970130     C                   Z-ADD     PS88          SPA88A
274900970130     C                   Z-ADD     PB87          SPA87B
275000970130     C                   Z-ADD     PS87          SPA87A
275100941012      * ---- PESI ----
275200090915     C                   Z-ADD     SKD(11)       SPAPFC
275300090915     C                   Z-ADD     SKD(12)       SPAPFE
275400090914     C                   Z-ADD     SKD(13)       SPAPFD
275500090914     C                   Z-ADD     SKD(14)       SPAPFh
275600090921     C                   Z-ADD     SKE(11)       SPAPOC
275700090921     C                   Z-ADD     SKE(12)       SPAPOE
275800090914     C                   Z-ADD     SKE(13)       SPAPOD
275900090914     C                   Z-ADD     SKE(14)       SPAPOh
276000941012     C                   Z-ADD     *ZERO         SPAPTA
276100941012      * ---- VOLUMI ----
276200090915     C                   Z-ADD     SKI(11)       SPAVPC
276300090915     C                   Z-ADD     SKI(12)       SPAVPE
276400090914     C                   Z-ADD     SKI(13)       SPAVPD
276500090914     C                   Z-ADD     SKI(14)       SPAVPh
276600941012      * ---- SPEDIZIONI --
276700090915     C                   Z-ADD     SKL(11)       SPASPC
276800090915     C                   Z-ADD     SKL(12)       SPASPE
276900090914     C                   Z-ADD     SKL(13)       SPASPD
277000090914     C                   Z-ADD     SKL(14)       SPASPh
277100090915     C                   Z-ADD     SKM(11)       SPASDC
277200090915     C                   Z-ADD     SKM(12)       SPASDE
277300090914     C                   Z-ADD     SKM(13)       SPASDD
277400090914     C                   Z-ADD     SKM(14)       SPASDh
277500090915     C                   Z-ADD     SKO(11)       SPASFC
277600090915     C                   Z-ADD     SKO(12)       SPASFE
277700090914     C                   Z-ADD     SKO(13)       SPASFD
277800090914     C                   Z-ADD     SKO(14)       SPASFh
277900090915     C                   Z-ADD     SKP(11)       SPATFC
278000090915     C                   Z-ADD     SKP(12)       SPATFE
278100090914     C                   Z-ADD     SKP(13)       SPATFD
278200090914     C                   Z-ADD     SKP(14)       SPATFh
278300040921     c                   clear                   spaftr
278400040921     c                   move      dateu         spadtr
278500941012      *
278600970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI NON SCRIVO NIENTE
278700970226     C     WFES          IFEQ      'F'
278800970226     C     SPACPE        ANDEQ     *ZEROS
278900970226     C     SPACPC        ANDEQ     *ZEROS
279000970226     C     SPACPD        ANDEQ     *ZEROS
279100970226     C                   ELSE
279200090911     C                   WRITE     FISPA000
279300970226     C                   END
279400941012      *
279500941012     C                   ENDSR
279600941012      ****************************************************************
279700941012      * Scrivo in archivio il riepilogo per transiti
279800941012      ****************************************************************
279900941012     C     WRITRA        BEGSR
280000941012      *
280100941012     C                   Z-ADD     SIMFEL        SPAFLE
280200941012     C                   Z-ADD     SIMFEL        SPALNP
280300950210     C                   Z-ADD     DS1ADA        SPADRE
280400941012     C                   MOVE      'T'           SPATRE
280500941012      * ---- COLLI ----
280600970130     C                   Z-ADD     XKA(2)        SPACPE
280700970130     C                   Z-ADD     XKA(1)        SPACPC
280800970130     C                   Z-ADD     XKA(3)        SPACPD
280900090914     C                   Z-ADD     XKA(4)        SPACPh
281000970130     C* N.B.:Nei transiti non si distinguono i colli disk B dagli altri
281100970130     C                   MOVE      *ZEROS        SPA88B
281200970130     C                   Z-ADD     TRPI88        SPA88A
281300970130     C                   MOVE      *ZEROS        SPA87B
281400970130     C                   Z-ADD     TRPI87        SPA87A
281500941012      * ---- PESI ----
281600941012     C                   Z-ADD     XKD(2)        SPAPFE
281700970203     C                   Z-ADD     XKD(1)        SPAPFC
281800970203     C                   Z-ADD     XKD(3)        SPAPFD
281900090914     C                   Z-ADD     XKD(4)        SPAPFh
282000970130     C                   Z-ADD     XKE(2)        SPAPOE
282100970130     C                   Z-ADD     XKE(1)        SPAPOC
282200970130     C                   Z-ADD     XKE(3)        SPAPOD
282300090914     C                   Z-ADD     XKE(4)        SPAPOh
282400941012     C                   Z-ADD     TRPETS        SPAPTA
282500941012      * ---- VOLUMI ----
282600970130     C                   Z-ADD     XKI(2)        SPAVPE
282700970130     C                   Z-ADD     XKI(1)        SPAVPC
282800970130     C                   Z-ADD     XKI(3)        SPAVPD
282900090914     C                   Z-ADD     XKI(4)        SPAVPh
283000941012      * ---- SPEDIZIONI --
283100970130     C                   Z-ADD     XKL(2)        SPASPE
283200970130     C                   Z-ADD     XKL(1)        SPASPC
283300970130     C                   Z-ADD     XKL(3)        SPASPD
283400090914     C                   Z-ADD     XKL(4)        SPASPh
283500970130     C                   Z-ADD     *ZERO         SPASDE
283600970130     C                   Z-ADD     *ZERO         SPASDC
283700970130     C                   Z-ADD     *ZERO         SPASDD
283800970130     C                   Z-ADD     *ZERO         SPASFE
283900970130     C                   Z-ADD     *ZERO         SPASFC
284000970130     C                   Z-ADD     *ZERO         SPASFD
284100941012      *
284200040921     c                   clear                   spaftr
284300040921     c                   z-add     dateu         spadtr
284400941012      *
284500970130     C     SPACPE        IFGT      *ZERO
284600970130     C     SPACPC        ORGT      *ZERO
284700970130     C     SPACPD        ORGT      *ZERO
284800090911     C                   WRITE     FISPA000
284900941012     C                   ENDIF
285000941012      *
285100941012     C                   ENDSR
285200941012      ****************************************************************
285300090911      * Routine di RISTAMPA: viene semplicemente letto il file FISPA00F
285400941012      * e stampato, senza alcuna elaborazione di dati.
285500941012      * E' quindi possibile ristampare piu' giorni accorpandoli insieme.
285600941012      * Le routine di RISTAMPA sono simili a quelle del pgm FNLSC2R.
285700941012      ****************************************************************
285800941012     C     RISTAM        BEGSR
285900050829     c                   setoff                                       3534
286000941012      *
286100941012      * Verifico se richiesta una specifica area
286200950210     C     DS1FEL        COMP      *ZERO                                  34
286300050829     c
286400050829     c* Se sono in filiale imposto = simfel
286500050829     c                   if        ds1fel=0 and simfel>0
286600050829     c                   eval      ds1fel=simfel
286700050829     c                   setoff                                       34
286800050829     c                   endif
286900050829     c
287000050829     c* Se devo elaborare una sola linea di partenza, leggo con
287100050829     c*  la linea
287200050829     c                   if        lin(2)=0 and lin(1)>0
287300050829     c                   eval      klin=lin(1)
287400050829     c                   eval      kdre=ds1ada
287500050829     c                   seton                                        35
287600050829     c                   endif
287700050829     c
287800020813     C                   MOVE      *ZEROS        XD
287900941012      *
288000941012     C                   Z-ADD     *ZERO         $PRIM1            1 0
288100941012     C                   Z-ADD     *ZERO         $FILPR            3 0
288200941012     C                   Z-ADD     *ZERO         $FI2PR            3 0
288300941012      *
288400090911     C  n35DS1FEL        SETLL     FISPA01L
288500090911     C   35kspa          SETLL     FISPA01L
288600941012     C                   DO        *HIVAL
288700050829     c* 35 on - lettura con lnp
288800050829     c                   if        *in35
288900090911     C     kspa2         READE(N)  FISPA01L                               33
289000050829     c                   else
289100090911     C  N34DS1FEL        READE(N)  FISPA01L                               33
289200090911     C   34              READ(N)   FISPA01L                               33
289300050829     c                   endif
289400941012      *
289500970130     C  N33SPADRE        IFGE      DS1ADA
289600970130     C     SPADRE        ANDLE     DS1AAL
289700941012      *
289800941012      * La linea deve essere presente in LIN
289900050829     c                   if        not *in35 and *in34
290000941012     C     SPALNP        LOOKUP    LIN                                    32
290100050829     c                   else
290200050829     c                   seton                                        32
290300050829     c                   endif
290400941012      *
290500941012     C     *IN32         IFEQ      *ON
290600941012      *
290700941012     C     SPATRE        IFEQ      *BLANK
290800941012      *
290900941012      * Al primo giro memorizzo il "precedente"
291000941012     C     $PRIM1        IFEQ      *ZERO
291100941012     C                   Z-ADD     SPAFLE        $FI2PR
291200941012     C                   Z-ADD     SPALNP        $FILPR
291300941012     C                   Z-ADD     1             $PRIM1
291400941012     C                   ENDIF
291500941012      *
291600941012      * Se cambia filiale stampo i totali per filiale
291700941012     C     SPALNP        IFNE      $FILPR
291800020813     C                   ADD       1             XD
291900941012     C                   Z-ADD     $FILPR        COMFIL
292000941012     C                   MOVE      *BLANKS       DESLNP
292100941012     C     $FILPR        CHAIN     AZORG01L                           99
292200970203     C     *IN99         IFEQ      *OFF
292300970203     C     ORGDE5        IFNE      *BLANKS
292400970203     C                   MOVEL     ORGDE5        DESLNP
292500970203     C                   ELSE
292600970203     C                   MOVEL     ORGDES        DESLNP
292700970203     C                   END
292800970203     C                   END
292900941012     C                   EXSR      TOTLNP
293000941012     C                   Z-ADD     SPALNP        $FILPR
293100941012     C                   ENDIF
293200941012      *
293300941012      * Se cambia area stampo i totali per c/servizio
293400941012     C     SPAFLE        IFNE      $FI2PR
293500941012     C                   EXSR      TOTCS
293600941012     C                   EXSR      TOTARE
293700000308     C  N08              XFOOT     XKA           TOTXKA            7 0
293800000308     C     *IN08         IFEQ      *ON
293900000308     C     TOTXKA        ORGT      0
294000000308     C                   EXSR      TOTTRA
294100000308     C                   ENDIF
294200941012     C                   Z-ADD     SPAFLE        $FI2PR
294300941012     C                   ENDIF
294400000308      *
294500941012     C                   ENDIF
294600941012      *
294700941012      * Aggiorno i totali
294800941012     C                   EXSR      SERTRA
294900941012      *
295000941012     C                   ENDIF
295100941012     C                   ENDIF
295200941012      *
295300941012     C  N33              ENDDO
295400941012      *
295500941012      * Stampo l'ultimo totale filiale ed il c/servizio
295600941012     C                   Z-ADD     $FILPR        COMFIL
295700941012     C                   MOVE      *BLANKS       DESLNP
295800941012     C     $FILPR        CHAIN     AZORG01L                           99
295900970203     C     *IN99         IFEQ      *OFF
296000970203     C     ORGDE5        IFNE      *BLANKS
296100970203     C                   MOVEL     ORGDE5        DESLNP
296200970203     C                   ELSE
296300970203     C                   MOVEL     ORGDES        DESLNP
296400970203     C                   END
296500970203     C                   END
296600941012     C                   EXSR      TOTLNP
296700941012     C                   EXSR      TOTCS
296800941012     C                   EXSR      TOTARE
296900941012      *
297000941012      * Stampo il transito
297100000308     C  N08              XFOOT     XKA           TOTXKA            7 0
297200000308     C     *IN08         IFEQ      *ON
297300000308     C     TOTXKA        ORGT      0
297400000308     C                   EXSR      TOTTRA
297500000308     C                   ENDIF
297600941012      *
297700941012     C     ENDRIS        ENDSR
297800941012      *****************************************************************
297900941012      * RISTAMPA: mantengo aggiornati i totali
298000941012      *****************************************************************
298100941012     C     SERTRA        BEGSR
298200941012      *
298300941012     C     SPATRE        IFEQ      *BLANK
298400941012      * Colli
298500090914     C                   ADD       SPACPC        SKA(6)
298600090914     C                   ADD       SPACPE        SKA(7)
298700090914     C                   ADD       SPACPD        SKA(8)
298800090914     C                   ADD       SPACPh        SKA(9)
298900970130     C                   ADD       SPA88B        PB88F
299000970130     C                   ADD       SPA88A        PI88
299100970130     C                   ADD       SPA87B        PB87F
299200970130     C                   ADD       SPA87A        PI87
299300941012      * Pesi
299400090914     C                   ADD       SPAPFC        SKD(6)
299500090914     C                   ADD       SPAPFE        SKD(7)
299600090914     C                   ADD       SPAPFD        SKD(8)
299700090914     C                   ADD       SPAPFh        SKD(9)
299800090914
299900090914     C                   ADD       SPAPOC        SKE(6)
300000090914     C                   ADD       SPAPOE        SKE(7)
300100090914     C                   ADD       SPAPOD        SKE(8)
300200090914     C                   ADD       SPAPOh        SKE(9)
300300090914     c
300400090914     C     SKD(6)        ADD       SKE(6)        SKF(6)
300500090914     C     SKD(7)        ADD       SKE(7)        SKF(7)
300600090914     C     SKD(8)        ADD       SKE(8)        SKF(8)
300700090914     C     SKD(9)        ADD       SKE(9)        SKF(9)
300800941012     C                   ADD       SPAPTA        PETS
300900941012      * Volumi
301000090914     C                   ADD       SPAVPC        SKI(6)
301100090914     C                   ADD       SPAVPE        SKI(7)
301200090914     C                   ADD       SPAVPD        SKI(8)
301300090914     C                   ADD       SPAVPh        SKI(9)
301400941012      * Spedizioni
301500090914     C                   ADD       SPASPC        SKL(6)
301600090914     C                   ADD       SPASPE        SKL(7)
301700090914     C                   ADD       SPASPD        SKL(8)
301800090914     C                   ADD       SPASPh        SKL(9)
301900090914
302000090914     C                   ADD       SPASDC        SKM(6)
302100090914     C                   ADD       SPASDE        SKM(7)
302200090914     C                   ADD       SPASDD        SKM(8)
302300090914     C                   ADD       SPASDh        SKM(9)
302400090914
302500090914     C     SPASFC        ADD       SPATFC        SKN(6)
302600090914     C     SPASFE        ADD       SPATFE        SKN(7)
302700090914     C     SPASFD        ADD       SPATFD        SKN(8)
302800090914     C     SPASFh        ADD       SPATFh        SKN(9)
302900941012     C                   ENDIF
303000941012      *
303100941012     C     SPATRE        IFEQ      'C'
303200941012      * Colli
303300970130     C                   ADD       SPACPC        SKA(1)
303400970130     C                   ADD       SPACPE        SKA(2)
303500970130     C                   ADD       SPACPD        SKA(3)
303600090914     C                   ADD       SPACPh        SKA(4)
303700970130     C                   ADD       SPA88B        PB88CS
303800970130     C                   ADD       SPA88B        PB88
303900970130     C                   ADD       SPA88A        PI88CS
304000970130     C                   ADD       SPA88A        PS88
304100970130     C                   ADD       SPA87B        PB87CS
304200970130     C                   ADD       SPA87B        PB87
304300970130     C                   ADD       SPA87A        PI87CS
304400970130     C                   ADD       SPA87A        PS87
304500941012      * Pesi
304600970130     C                   ADD       SPAPFC        SKD(1)
304700970130     C                   ADD       SPAPFE        SKD(2)
304800970130     C                   ADD       SPAPFD        SKD(3)
304900090914     C                   ADD       SPAPFh        SKD(4)
305000970130     C                   ADD       SPAPOC        SKE(1)
305100970130     C                   ADD       SPAPOE        SKE(2)
305200970130     C                   ADD       SPAPOD        SKE(3)
305300090914     C                   ADD       SPAPOh        SKE(4)
305400970130     C     SKD(1)        ADD       SKE(1)        SKF(1)
305500970130     C     SKD(2)        ADD       SKE(2)        SKF(2)
305600970130     C     SKD(3)        ADD       SKE(3)        SKF(3)
305700090914     C     SKD(4)        ADD       SKE(4)        SKF(4)
305800941012      * Volumi
305900970130     C                   ADD       SPAVPC        SKI(1)
306000970130     C                   ADD       SPAVPE        SKI(2)
306100970130     C                   ADD       SPAVPD        SKI(3)
306200090914     C                   ADD       SPAVPh        SKI(4)
306300941012      * Spedizioni
306400970130     C                   ADD       SPASPC        SKL(1)
306500970130     C                   ADD       SPASPE        SKL(2)
306600970130     C                   ADD       SPASPD        SKL(3)
306700090914     C                   ADD       SPASPh        SKL(4)
306800970130     C                   ADD       SPASDC        SKM(1)
306900970130     C                   ADD       SPASDE        SKM(2)
307000970130     C                   ADD       SPASDD        SKM(3)
307100090914     C                   ADD       SPASDh        SKM(4)
307200970228     C     SPATFC        ADD       SPASFC        W0010
307300970228     C     SPATFE        ADD       SPASFE        W0020
307400970228     C     SPATFD        ADD       SPASFD        W0030
307500090914     C     SPATFh        ADD       SPASFh        W0040
307600970228     C                   ADD       W0010         SKN(1)
307700970228     C                   ADD       W0020         SKN(2)
307800970228     C                   ADD       W0030         SKN(3)
307900090914     C                   ADD       W0040         SKN(4)
308000941012     C                   ENDIF
308100941012      *
308200941012     C     SPATRE        IFEQ      'T'
308300970130     C                   ADD       SPACPC        XKA(1)
308400970130     C                   ADD       SPACPE        XKA(2)
308500970130     C                   ADD       SPACPD        XKA(3)
308600090914     C                   ADD       SPACPh        XKA(4)
308700970130     C                   ADD       SPA88A        TRPI88
308800970130     C                   ADD       SPA87A        TRPI87
308900941012      * Pesi
309000970130     C                   ADD       SPAPFC        XKD(1)
309100970130     C                   ADD       SPAPFE        XKD(2)
309200970130     C                   ADD       SPAPFD        XKD(3)
309300090914     C                   ADD       SPAPFh        XKD(4)
309400970130     C                   ADD       SPAPOC        XKE(1)
309500970130     C                   ADD       SPAPOE        XKE(2)
309600970130     C                   ADD       SPAPOD        XKE(3)
309700090914     C                   ADD       SPAPOh        XKE(4)
309800970228     C     XKD(1)        ADD       XKE(1)        XKF(1)
309900970228     C     XKD(2)        ADD       XKE(2)        XKF(2)
310000970228     C     XKD(3)        ADD       XKE(3)        XKF(3)
310100090914     C     XKD(4)        ADD       XKE(4)        XKF(4)
310200941012     C                   ADD       SPAPTA        TRPETS
310300941012      * Volumi
310400970130     C                   ADD       SPAVPC        XKI(1)
310500970130     C                   ADD       SPAVPE        XKI(2)
310600970130     C                   ADD       SPAVPD        XKI(3)
310700090914     C                   ADD       SPAVPh        XKI(4)
310800941012      * Spedizioni
310900970130     C                   ADD       SPASPC        XKL(1)
311000970130     C                   ADD       SPASPE        XKL(2)
311100970130     C                   ADD       SPASPD        XKL(3)
311200090914     C                   ADD       SPASPh        XKL(4)
311300941012     C                   ENDIF
311400941012      *
311500941012     C                   ENDSR
311600941012      ****************************************************************
311700941012      * Verifica se vi sono dei fogli viaggio con data partenza compresa
311800941012      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
311900941012      ****************************************************************
312000941012     C     FOGVIA        BEGSR
312100941012      *
312200950404     C  N36DS1RIS        IFEQ      'S'
312300941012      *
312400020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
312500020813     ***C     *LOVAL        SETLL     FNFGV04L
312600020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
312700020813     C     SIMfel        setll     FNFGV06L
312800941012     C                   DO        *HIVAL
312900020813     ***C                   READ      FNFGV04L                               31
313000020813     C     SIMfel        reade     FNFGV06L                               31
313100950210     C  N31FGVDTP        IFGE      DS1ADA
313200950210     C     FGVDTP        ANDLE     DS1AAL
313300941012     C                   SETON                                        31
313400941012     C                   EXCEPT    FVAPER
313500941012     C                   ENDIF
313600941012     C  N31              ENDDO
313700941012      *
313800941012     C                   ENDIF
313900941012      *
314000941012     C                   ENDSR
314100940103      ****************************************************************
314200941012      * ----------------?    TESTATA  DI  STAMPA    ?-----------------
314300930928     O*PRIMA RIGA SE C'E' SOLO 'DATA DA'
314400930928     OPRTF198   E   N03      TESTA            02
314500930928     O                       SK(1)               66
314600930928     O                       SK(2)              132
314700930928     O                       SK(4)              198
314800930928     O                       RSUT                20
314900941012     O                       STRIP               48
315000950210     O                       DS1GDA             113 '  /  /    '
315100950210     O                       WDATE              181 '  /  /    '
315200930928     O                       PAGE          Z    198
315300930928     O*PRIMA RIGA SE C'E' 'DATA DA' E 'DATA A'
315400930928     O          E    03      TESTA            02
315500930928     O                       SK(1)               66
315600930928     O                       SK(3)              132
315700930928     O                       SK(4)              198
315800930928     O                       RSUT                20
315900941012     O                       STRIP               48
316000950210     O                       DS1GDA             113 '  /  /    '
316100950210     O                       DS1GAL             128 '  /  /    '
316200950210     O                       WDATE              181 '  /  /    '
316300930928     O                       PAGE          Z    198
316400930928     O*RIGA IN CUI C'E' SOLO UTIME
316500930928     O          E            TESTA            03
316600950111     O                       KNSIF               10
316700950111     O                       KNMUS               21
316800941012     O                       STRIP1              48
316900950210     O                       UTIME              180 '  :  :  '
317000930928     O*TRATTINI
317100930928     O          E            TESTA            04
317200930928     O                       SK(5)               66
317300930928     O                       SK(5)              132
317400970306     O                       SK(18)             198
317500930928     O*PRIMA RIGA DI COSTANTI
317600930928     O          E            TESTA            05
317700930928     O                       SK(6)               66
317800970131     O                       SK(9)              132
317900970131     O                       SK(12)             198
318000930928     O*SECONDA RIGA DI COSTANTI
318100930928     O          E            TESTA            06
318200970131     O                       SK(7)               66
318300930928     O                       SK(10)             132
318400970131     O                       SK(13)             198
318500930928     O*TERZA RIGA DI COSTANTI
318600930928     O          E            TESTA            07
318700970131     O                       SK(8)               66
318800970131     O                       SK(11)             132
318900930928     O                       SK(14)             198
319000930928     O*TRATTINI
319100970131     O          E            TESTA            08
319200930928     O                       SK(5)               66
319300930928     O                       SK(5)              132
319400970306     O                       SK(18)             198
319500941012      * ----------------?    DETTAGLIO DI FILIALE    ?----------------
319600930928     O****1°RIGA DEL DETTAGLIO FILIALE = TOTALI
319700930928     O**COLLI
319800930929     O          E            DETFIL      2
319900970131     O                       DESLNP              10
320000970131     O                       SKA456        Z     18
320100970131     O*COLLI NO SPUNT. IN PART. DISK B
320200970131     O                       PB88F         Z     26
320300970131     O                       PC88B         4     33
320400970131     O                       PER3B               34
320500970131     O*COLLI NO SPUNT. IN PART. ALTRI
320600970131     O                       PI88          Z     42
320700970131     O                       PC88          4     49
320800970131     O                       PER3                50
320900970131     O*COLLI NO SPUNT. IN PART. TOTALE
321000970131     O                       PI88T         Z     58
321100970131     O                       PC88T         4     65
321200970131     O                       PER3T               66
321300970131     O*COLLI PISTOLA 87 (EX 88) DISK B
321400970131     O                       PB87F         Z     74
321500970131     O                       PC87B         4     81
321600970131     O                       PER34B              82
321700970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
321800970131     O                       PI87          Z     90
321900970131     O                       PC87          4     97
322000970131     O                       PER34               98
322100970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
322200970131     O                       PI87T         Z    106
322300970131     O                       PC87T         4    113
322400970131     O                       PER34T             114
322500930928     O**PESO
322600970131     O                       SKD456        4    124
322700970131     O                       SKE456        4    134
322800970131     O                       SKF456        4    144
322900930928     O**PESO TASSABILE
323000970131     O                       PETS          4    155
323100930928     O**VOLUME
323200970131     O                       SKI456        4    165
323300930928     O**SPEDIZIONI
323400970131     O                       SKL456        Z    174
323500970131     O                       SKM456        Z    182
323600090914     O*
323700090914     O****  RIGA DEL DETTAGLIO FILIALE = "H" - h1030
323800090914     O**COLLI
323900090914     O          E    10      DETFIL      1
324000090914     o                       wdtsp(04)           11
324100090914     O                       SKA(9)        Z     18
324200090914     O**PESO
324300090914     O                       SKD(9)        4    124
324400090914     O                       SKE(9)        4    134
324500090914     O                       SKF(9)        4    144
324600090914     O**VOLUME
324700090914     O                       SKI(9)        4    165
324800090914     O**SPEDIZIONI
324900090914     O                       SKL(9)        Z    174
325000090914     O                       SKM(9)        Z    182
325100090914     O*
325200930928     O*
325300090914     O****2°RIGA DEL DETTAGLIO FILIALE = "E" - priority
325400930928     O**COLLI
325500930929     O          E            DETFIL      1
325600090605     O**!!!                                      11 'ESPR'
325700090605     o                       wdtsp(02)           11
325800090914     O                       SKA(7)        Z     18
325900930928     O**PESO
326000090914     O                       SKD(7)        4    124
326100090914     O                       SKE(7)        4    134
326200090914     O                       SKF(7)        4    144
326300930928     O**VOLUME
326400090914     O                       SKI(7)        4    165
326500930928     O**SPEDIZIONI
326600090914     O                       SKL(7)        Z    174
326700090914     O                       SKM(7)        Z    182
326800931011     O*
326900090914     O****3°RIGA DEL DETTAGLIO FILIALE = "D" - DISTRIBUZIONE
327000930928     O**COLLI
327100930929     O          E            DETFIL      1
327200090605     O**!!!                                      11 'DIST'
327300090605     o                       wdtsp(03)           11
327400090914     O                       SKA(8)        Z     18
327500930928     O**PESO
327600090914     O                       SKD(8)        4    124
327700090914     O                       SKE(8)        4    134
327800090914     O                       SKF(8)        4    144
327900930928     O**VOLUME
328000090914     O                       SKI(8)        4    165
328100930928     O**SPEDIZIONI
328200090914     O                       SKL(8)        Z    174
328300090914     O                       SKM(8)        Z    182
328400940323     O*
328500090914     O****4°RIGA DEL DETTAGLIO FILIALE = "C" - express
328600940323     O**COLLI
328700940323     O          E            DETFIL      1
328800090605     O**!!!                                      11 'CORR'
328900090605     o                       wdtsp(01)           11
329000090914     O                       SKA(6)        Z     18
329100940323     O**PESO
329200090914     O                       SKD(6)        4    124
329300090914     O                       SKE(6)        4    134
329400090914     O                       SKF(6)        4    144
329500940323     O**VOLUME
329600090914     O                       SKI(6)        4    165
329700940323     O**SPEDIZIONI
329800090914     O                       SKL(6)        Z    174
329900090914     O                       SKM(6)        Z    182
330000941012      * ----------?    RIEPILOGO C/SERVIZI PER FILIALE    ?-----------
330100930929     O****1°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = TOTALI
330200930929     O**COLLI
330300970131     O          E            DETCS       2
330400970131     O                                           10 'C/SERVIZI '
330500970131     O                       SKA123        Z     18
330600970131     O*COLLI NO SPUNT. IN PART. DISK B
330700970131     O                       PB88CS        Z     26
330800970131     O                       PC88CB        4     33
330900970131     O                       PER3B               34
331000970131     O*COLLI NO SPUNT. IN PART. ALTRI
331100970131     O                       PI88CS        Z     42
331200970131     O                       PC88CS        4     49
331300970131     O                       PER3                50
331400970131     O*COLLI NO SPUNT. IN PART. TOTALE
331500970131     O                       P88CST        Z     58
331600970131     O                       PC88CT        4     65
331700970131     O                       PER3T               66
331800970131     O*COLLI PISTOLA 87 (EX 88) DISKB
331900970131     O                       PB87CS        Z     74
332000970131     O                       PC87CB        4     81
332100970131     O                       PER34B              82
332200970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
332300970131     O                       PI87CS        Z     90
332400970131     O                       PC87CS        4     97
332500970131     O                       PER34               98
332600970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
332700970131     O                       P87CST        Z    106
332800970131     O                       PC87CT        4    113
332900970131     O                       PER34T             114
333000930929     O**PESO
333100970131     O                       SKD123        4    124
333200970131     O                       SKE123        4    134
333300970131     O                       SKF123        4    144
333400930929     O**PESO TASSABILE (PER C/SERVIZI NON C'E').
333500930929     O**VOLUME
333600970131     O                       SKI123        4    165
333700930929     O**SPEDIZIONI
333800970131     O                       SKL123        Z    174
333900970131     O                       SKM123        Z    182
334000930929     O*
334100090914     O****  RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "H" - h 1030
334200090914     O**COLLI
334300090914     O          E    10      DETCS       1
334400090914     o                       wdtsp(04)           11
334500090914     O                       SKA(4)        Z     18
334600090914     O**PESO
334700090914     O                       SKD(4)        4    124
334800090914     O                       SKE(4)        4    134
334900090914     O                       SKF(4)        4    144
335000090914     O**VOLUME
335100090914     O                       SKI(4)        4    165
335200090914     O**SPEDIZIONI
335300090914     O                       SKL(4)        Z    174
335400090914     O                       SKM(4)        Z    182
335500090914     O*
335600090914     O****2°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "E" - priority
335700930929     O**COLLI
335800930929     O          E            DETCS       1
335900090605     O**!!!                                      11 'ESPR'
336000090605     o                       wdtsp(02)           11
336100970131     O                       SKA(2)        Z     18
336200930929     O**PESO
336300970131     O                       SKD(2)        4    124
336400970131     O                       SKE(2)        4    134
336500970131     O                       SKF(2)        4    144
336600930929     O**VOLUME
336700970131     O                       SKI(2)        4    165
336800930929     O**SPEDIZIONI
336900970131     O                       SKL(2)        Z    174
337000970131     O                       SKM(2)        Z    182
337100931011     O*
337200090914     O****3°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "D" - DISTRIBUZIONE
337300930929     O**COLLI
337400930929     O          E            DETCS       1
337500090605     O**!!!                                      11 'DIST'
337600090605     o                       wdtsp(03)           11
337700970131     O                       SKA(3)        Z     18
337800930929     O**PESO
337900970131     O                       SKD(3)        4    124
338000970131     O                       SKE(3)        4    134
338100970131     O                       SKF(3)        4    144
338200930929     O**VOLUME
338300970131     O                       SKI(3)        4    165
338400930929     O**SPEDIZIONI
338500970131     O                       SKL(3)        Z    174
338600970131     O                       SKM(3)        Z    182
338700940323     O*
338800090914     O****4°RIGA DEL DETTAGLIO FILIALE C/SERVIZI= "C"- express
338900940323     O**COLLI
339000940323     O          E            DETCS       1
339100090605     O**!!!                                      11 'CORR'
339200090605     o                       wdtsp(01)           11
339300970131     O                       SKA(1)        Z     18
339400940323     O**PESO
339500970131     O                       SKD(1)        4    124
339600970131     O                       SKE(1)        4    134
339700970131     O                       SKF(1)        4    144
339800940323     O**VOLUME
339900970131     O                       SKI(1)        4    165
340000940323     O**SPEDIZIONI
340100970131     O                       SKL(1)        Z    174
340200970131     O                       SKM(1)        Z    182
340300941012      * ----------------?    RIEPILOGO PER AREA    ?------------------
340400941012     O*TRATTINI DI SEPARAZIONE TRA I  C/SERVIZI E L'AREA
340500941012     O          E            DETARE      1
340600941012     O                       SK(5)               66
340700941012     O                       SK(5)              132
340800970306     O                       SK(18)             198
340900090914     o
341000930930     O****1°RIGA DEL DETTAGLIO AREA = TOTALI
341100930930     O**COLLI
341200931011     O          E            DETARE      1
341300970131     O                                           10 'TOTALE    '
341400970131     O                       AKA123        Z     18
341500970131     O*COLLI NO SPUNT. IN PART. DISK B
341600970131     O                       ARPB88        Z     26
341700970131     O                       APC88B        4     33
341800970131     O                       PER3B               34
341900970131     O*COLLI NO SPUNT. IN PART. ALTRI
342000970131     O                       ARPI88        Z     42
342100970131     O                       ARPC88        4     49
342200970131     O                       PER3                50
342300970131     O*COLLI NO SPUNT. IN PART. TOTALE
342400970131     O                       ARP88T        Z     58
342500970131     O                       APC88T        4     65
342600970131     O                       PER3T               66
342700970131     O*COLLI PISTOLA 87 (EX 88) DISK B
342800970131     O                       ARPB87        Z     74
342900970131     O                       APC87B        4     81
343000970131     O                       PER34B              82
343100970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
343200970131     O                       ARPI87        Z     90
343300970131     O                       ARPC87        4     97
343400970131     O                       PER34               98
343500970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
343600970131     O                       ARP87T        Z    106
343700970131     O                       APC87T        4    113
343800970131     O                       PER34T             114
343900930930     O**PESO
344000970131     O                       AKD123        4    124
344100970131     O                       AKE123        4    134
344200970131     O                       AKF123        4    144
344300930930     O**PESO TASSABILE
344400970131     O                       ARPETS        4    155
344500930930     O**VOLUME
344600970131     O                       AKI123        4    165
344700930930     O**SPEDIZIONI
344800970131     O                       AKL123        Z    174
344900970131     O                       AKM123        Z    182
345000930930     O*
345100090914     O****  RIGA DEL DETTAGLIO AREA = "H"-h10 30
345200930930     O**COLLI
345300090914     O          E    10      DETARE      1
345400090914     o                       wdtsp(04)           11
345500090914     O                       AKA(4)        Z     18
345600930930     O**PESO
345700090914     O                       AKD(4)        4    124
345800090914     O                       AKE(4)        4    134
345900090914     O                       AKF(4)        4    144
346000930930     O**VOLUME
346100090914     O                       AKI(4)        4    165
346200930930     O**SPEDIZIONI
346300090914     O                       AKL(4)        Z    174
346400090914     O                       AKM(4)        Z    182
346500090914     O*
346600090914     O****2°RIGA DEL DETTAGLIO AREA = "E"-priority
346700090914     O**COLLI
346800090914     O          E            DETARE      1
346900090914     O**!!!                                      11 'ESPR'
347000090914     o                       wdtsp(02)           11
347100090914     O                       AKA(2)        Z     18
347200090914     O**PESO
347300090914     O                       AKD(2)        4    124
347400090914     O                       AKE(2)        4    134
347500090914     O                       AKF(2)        4    144
347600090914     O**VOLUME
347700090914     O                       AKI(2)        4    165
347800090914     O**SPEDIZIONI
347900090914     O                       AKL(2)        Z    174
348000090914     O                       AKM(2)        Z    182
348100931011     O*
348200090914     O****3°RIGA DEL DETTAGLIO AREA = "D" -DISTRIBUZIONE
348300930930     O**COLLI
348400940324     O          E            DETARE      1
348500090605     O**!!!                                      11 'DIST'
348600090605     o                       wdtsp(03)           11
348700970131     O                       AKA(3)        Z     18
348800930930     O**PESO
348900970131     O                       AKD(3)        4    124
349000970131     O                       AKE(3)        4    134
349100970131     O                       AKF(3)        4    144
349200930930     O**VOLUME
349300970131     O                       AKI(3)        4    165
349400930930     O**SPEDIZIONI
349500970131     O                       AKL(3)        Z    174
349600970131     O                       AKM(3)        Z    182
349700940323     O*
349800090914     O****4°RIGA DEL DETTAGLIO AREA = "C"- express
349900940323     O**COLLI
350000940324     O          E            DETARE      1  1
350100090605     O**!!!                                      11 'CORR'
350200090605     o                       wdtsp(01)           11
350300970203     O                       AKA(1)        Z     18
350400940323     O**PESO
350500970203     O                       AKD(1)        4    124
350600970203     O                       AKE(1)        4    134
350700970203     O                       AKF(1)        4    144
350800940323     O**VOLUME
350900970203     O                       AKI(1)        4    165
351000940323     O**SPEDIZIONI
351100970203     O                       AKL(1)        Z    174
351200970203     O                       AKM(1)        Z    182
351300941012      * ----------------?    RIEPILOGO TRANSITI    ?------------------
351400931008     O****1°RIGA DEL DETTAGLIO TRANSITI = TOTALI
351500931008     O**COLLI
351600970203     O          E            DETTRA      1
351700970203     O                                           10 'TRANSITI  '
351800970203     O                       XKA123        Z     18
351900931008     O*COLLI NO SPUNT. IN PART.
352000970203     O                       TRPI88        Z     58
352100970203     O                       TRPC88        4     65
352200970203     O                       PER3                66
352300950622     O*COLLI PISTOLA 87 (EX 88 E 89)
352400970203     O                       TRPI87        Z    106
352500970203     O                       TRPC87        4    113
352600970203     O                       PER34              114
352700931008     O**PESO
352800970203     O                       XKD123        4    124
352900970203     O                       XKE123        4    134
353000970203     O                       XKF123        4    144
353100931008     O**PESO TASSABILE
353200970203     O                       TRPETS        4    155
353300931008     O**VOLUME
353400970203     O                       XKI123        4    165
353500931008     O**SPEDIZIONI
353600970203     O                       XKL123        Z    174
353700931008     O*
353800090914     O****  RIGA DEL DETTAGLIO TRANSITI = "H" - h10 30
353900090914     O**COLLI
354000090914     O          E    10      DETTRA      1
354100090914     o                       wdtsp(04)           11
354200090914     O                       XKA(4)        Z     18
354300090914     O**PESO
354400090914     O                       XKD(4)        4    124
354500090914     O                       XKE(4)        4    134
354600090914     O                       XKF(4)        4    144
354700090914     O**VOLUME
354800090914     O                       XKI(4)        4    165
354900090914     O**SPEDIZIONI
355000090914     O                       XKL(4)        Z    174
355100090914     O*
355200090914     O****2°RIGA DEL DETTAGLIO TRANSITI = "E" - priority
355300931008     O**COLLI
355400931008     O          E            DETTRA      1
355500090605     O**!!!                                      11 'ESPR'
355600090605     o                       wdtsp(02)           11
355700970203     O                       XKA(2)        Z     18
355800931008     O**PESO
355900970203     O                       XKD(2)        4    124
356000970203     O                       XKE(2)        4    134
356100970203     O                       XKF(2)        4    144
356200931008     O**VOLUME
356300970203     O                       XKI(2)        4    165
356400931008     O**SPEDIZIONI
356500970203     O                       XKL(2)        Z    174
356600931011     O*
356700090914     O****3°RIGA DEL DETTAGLIO TRANSITI = "D" -DISTRIBUZIONE
356800931008     O**COLLI
356900931008     O          E            DETTRA      1
357000090605     O**!!!                                      11 'DIST'
357100090605     o                       wdtsp(03)           11
357200970203     O                       XKA(3)        Z     18
357300931008     O**PESO
357400970203     O                       XKD(3)        4    124
357500970203     O                       XKE(3)        4    134
357600970203     O                       XKF(3)        4    144
357700931008     O**VOLUME
357800970203     O                       XKI(3)        4    165
357900931008     O**SPEDIZIONI
358000970203     O                       XKL(3)        Z    174
358100940323     O*
358200090914     O****4°RIGA DEL DETTAGLIO TRANSITI = "C" - express
358300940323     O**COLLI
358400940323     O          E            DETTRA      1
358500090605     O**!!!                                      11 'CORR'
358600090605     o                       wdtsp(01)           11
358700970203     O                       XKA(1)        Z     18
358800940323     O**PESO
358900970203     O                       XKD(1)        4    124
359000970203     O                       XKE(1)        4    134
359100970203     O                       XKF(1)        4    144
359200940323     O**VOLUME
359300970203     O                       XKI(1)        4    165
359400940323     O**SPEDIZIONI
359500970203     O                       XKL(1)        Z    174
359600941012      * ------------------?    TOTALI  GENERALI    ?------------------
359700940401     O*TRATTINI DI SEPARAZIONE TRA I TRANSITI ED IL TOTALE GENERALE.
359800941012     O          E            DETTOT      1
359900941012     O                       SK(5)               66
360000941012     O                       SK(5)              132
360100970306     O                       SK(18)             198
360200931008     O****1°RIGA DEL DETTAGLIO TOT.GEN. = TOTALI
360300931008     O**COLLI
360400970203     O          E            DETTOT      1
360500970203     O                                           10 'TOT.GENER.'
360600970203     O                       TKA123        Z     18
360700931008     O*COLLI NO SPUNT. IN PART.
360800970203     O                       TGP88T        Z     58
360900970203     O                       TGC88T        4     65
361000970203     O                       PER3                66
361100950622     O*COLLI PISTOLA 87 (EX 88 E 89)
361200970203     O                       TGP87T        Z    106
361300970203     O                       TGC87T        4    113
361400970203     O                       PER34              114
361500931008     O**PESO
361600970203     O                       TKD123        4    124
361700970203     O                       TKE123        4    134
361800970203     O                       TKF123        4    144
361900931008     O**PESO TASSABILE
362000970203     O                       TGPETS        4    155
362100931008     O**VOLUME
362200970203     O                       TKI123        4    165
362300931008     O**SPEDIZIONI
362400970203     O                       TKL123        Z    174
362500931008     O*
362600090914     O****  RIGA DEL DETTAGLIO TOT.GEN. = "H"-h1030
362700090914     O**COLLI
362800090914     O          E    10      DETTOT      1
362900090914     o                       wdtsp(04)           11
363000090914     O                       TKA(4)        Z     18
363100090914     O**PESO
363200090914     O                       TKD(4)        4    124
363300090914     O                       TKE(4)        4    134
363400090914     O                       TKF(4)        4    144
363500090914     O**VOLUME
363600090914     O                       TKI(4)        4    165
363700090914     O**SPEDIZIONI
363800090914     O                       TKL(4)        Z    174
363900090914     O*
364000090914     O****2°RIGA DEL DETTAGLIO TOT.GEN. = "E"-priority
364100931008     O**COLLI
364200931008     O          E            DETTOT      1
364300090605     O**!!!                                      11 'ESPR'
364400090605     o                       wdtsp(02)           11
364500970203     O                       TKA(2)        Z     18
364600931008     O**PESO
364700970203     O                       TKD(2)        4    124
364800970203     O                       TKE(2)        4    134
364900970203     O                       TKF(2)        4    144
365000931008     O**VOLUME
365100970203     O                       TKI(2)        4    165
365200931008     O**SPEDIZIONI
365300970203     O                       TKL(2)        Z    174
365400931008     O*
365500950111     O****3°RIGA DEL DETTAGLIO TOT.GEN. = DISTRIBUZIONE
365600931008     O**COLLI
365700931008     O          E            DETTOT      1
365800090605     O**!!!                                      11 'DIST'
365900090605     o                       wdtsp(03)           11
366000970203     O                       TKA(3)        Z     18
366100931008     O**PESO
366200970203     O                       TKD(3)        4    124
366300970203     O                       TKE(3)        4    134
366400970203     O                       TKF(3)        4    144
366500931008     O**VOLUME
366600970203     O                       TKI(3)        4    165
366700931008     O**SPEDIZIONI
366800970203     O                       TKL(3)        Z    174
366900940323     O*
367000950111     O****4°RIGA DEL DETTAGLIO TOT.GEN. = CORRIERE
367100940323     O**COLLI
367200940323     O          E            DETTOT      1
367300090605     O**!!!                                      11 'CORR'
367400090605     o                       wdtsp(01)           11
367500970203     O                       TKA(1)        Z     18
367600940323     O**PESO
367700970203     O                       TKD(1)        4    124
367800970203     O                       TKE(1)        4    134
367900970203     O                       TKF(1)        4    144
368000940323     O**VOLUME
368100970203     O                       TKI(1)        4    165
368200940323     O**SPEDIZIONI
368300970203     O                       TKL(1)        Z    174
368400931008     O*TRATTINI DI SEPARAZIONE DOPO IL TOTALE GENERALE.
368500931008     O          E            DETTOT      1  1
368600931008     O                       SK(5)               66
368700931008     O                       SK(5)              132
368800970306     O                       SK(18)             198
368900931130      *
369000941012     O          E            FINSTA           63
369100931130     O                       SK1(1)              66
369200941012     O          E            FINSTA           64
369300931130     O                       SK1(2)              66
369400931130     O                       SK1(3)             132
369500931130     O                       SK1(4)             198
369600950111     O                       §QTKPM        2     72
369700931130     O                       §3IARR        Z     99
369800941012     O          E            FINSTA           65
369900941012     O                       SK1(7)              66
370000940126      * AVVISO DI FOGLI VIAGGIO APERTI NEL PERIODO
370100941012     O          E            FVAPER      2
370200941012     O                       SK1(5)             115
370300941012     O                       SK1(6)            +  0
370400931008     O****************************************************************
370500931130      *            2         3         4         5         6         7         8
370600940406**   CMD           0123456789012345678901234567890123456789012345678901234567890
370700950209OVRPRTF FILE(PRTF198) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.part.
370800950209')
370900930928**   SK
371000050802XXXXXXXXXXXXXXXXXXXX                                                 1
371100950111   STATISTICA PARTENZE - PARTITO  IL XX/XX/XXXX                      2
371200950111   STATISTICA PARTENZE - PARTITO DAL XX/XX/XXXX  AL XX/XX/XXXX       3
371300970807                      FNLSC1R                             PAG.XXXX   4
371400930928------------------------------------------------------------------   5
371500970502  PUNTO     COLLI  |--- COLLI NON SPUNTATI (PIST.88,89,98,96) ---|
371600970204OPERATIVO            DiskB  % sul    Altri  % sul   Totale  % sul    7
371700970204                           partito         partito         partito   8
371800970131 |----- S P U N T E   P I S T O L A   87 ------| |----- P   E   S    9
371900970204   DiskB  % sul    Altri  % sul   Totale  % sul   FINO 35 OLTRE 35  10
372000970204         partito         partito         partito                    11
372100970306  O -------|     PESO      VOLUME    SPEDIZIONI                     12
372200970305 Q    TOTALE  TASSABILE            Totale  Tot.Da                   13
372300970305                                   Partito Partire                  14
3724009701311234567890 1234567 1234567 123,45% 1234567 123,45% 1234567 123,45%  15
372500970131 1234567 123,45% 1234567 123,45% 1234567 123,45% 1234567,8 1234567  16
372600970305,8 1234567,8 12345678,9 12345,6789 1234567 1234567 1234567 123,45%  17
372700970306--------------------------------------------------                  18
372800931130**
372900940405N.B.
373000940405PESO TASSABILE:  E' CALCOLATO CON UN RAPPORTO PESO/VOLUME  1 MC =
373100940405XX,XXX Q.LI ARROTONDATO AI XXXXXX KG. SUPERIORI, PER LE SOLE SPEDI
373200940405ZIONI CON PESO FINO A KG. 3500, ESCLUSE BOLLE DI RECUPERO E C/SERV
373300940126***   SONO STATI TROVATI ALCUNI FOGLI VIAGGIO APERTI AVENTI DATA P
373400940126ARTENZA COMPRESA NEL PERIODO   ***
373500940406SPEDIZIONI:  DALLA STATISTICA SONO ESLUSE LE BOLLE DI RECUPERO
