000100001205     H*------------------------------------------------------------------------*
000200001205     H* SIMULAZIONE CONTO ECONOMICO - SITEMA SALDI CLIENTE CON NOTE E RETTIFICHE
000300001205     H*------------------------------------------------------------------------*
000400001205     H DECEDIT('0,') DATEDIT(*DMY.)
000500001205     F*------------------------------------------------------------------------*
000600001205     F* DATA BASE
000700001205     F*------------------------------------------------------------------------*
000800001205     F*---
000900001205     F* Saldi cliente in BUDGET
001000001205     F*---
001100021216     FSISDOB    UF A E           K DISK
001200021216     F                                     RENAME(SISDO000:SISDOBDG)
001300001205     f                                     PREFIX(b_)
001400021216     FSISDCB    UF A E           K DISK
001500001205     F                                     RENAME(SISDC000:SISDCBDG)
001600001205     f                                     PREFIX(b_)
001700001205     F*---
001800021219     F* Saldi cliente in UNITRAGRU
001900001205     F*---
002000021219     FSISDOU    IF   E           K DISK
002100021219     F                                     RENAME(SISDO000:SISDOUNI)
002200021216     f                                     PREFIX(u_)
002300021219     FSISDCU    IF   E           K DISK
002400021216     F                                     RENAME(SISDC000:SISDCUNI)
002500021216     f                                     PREFIX(u_)
002600001205     D*------------------------------------------------------------------------*
002700001205     D* DATA STRUCTURE
002800001205     D*------------------------------------------------------------------------*
002900001205     D KDOKSC          S                   LIKE(B_SDOKSC)
003000001205     D KDOANN          S                   LIKE(B_SDOANN)
003100001205     D KDOMES          S                   LIKE(B_SDOMES)
003200001205     D KDCKSC          S                   LIKE(B_SDCKSC)
003300001205     D KDCANN          S                   LIKE(B_SDCANN)
003400001205     D KDCMES          S                   LIKE(B_SDCMES)
003500001205     D KDCREP          S                   LIKE(B_SDCREP)
003600001205     D KDCREA          S                   LIKE(B_SDCREA)
003700001205     D KDCFPE          S                   LIKE(B_SDCFPE)
003800001205     D KDCTSP          S                   LIKE(B_SDCTSP)
003900001205     D KDCCTR          S                   LIKE(B_SDCCTR)
004000001205     D KDCTPO          S                   LIKE(B_SDCTPO)
004100001205     D KDCRBL          S                   LIKE(B_SDCRBL)
004200001205     D KDCTPR          S                   LIKE(B_SDCTPR)
004300021216     D*-------------------
004400021216     D* ARCHITETTURA
004500021216     D*-------------------
004600021216     D KPJBA         E DS
004700021216     D*-------------------
004800021216     D* PARAMETRI IN ENTRATA
004900021216     D*-------------------
005000021216     D PARAM           DS
005100021216     D  PARORD                 1      1
005200021216     D  PARFIL                 2      4  0
005300021216     D  PARKS1                 5     11  0
005400021216     D  PARDS1                12     19  0
005500021216     D  PARAA1                12     15  0
005600021216     D  PARMG1                16     19  0
005700021218     D  PARME1                16     17  0
005800140530     D  PARgg1                18     19  0
005900021216     D  PARDS2                20     27  0
006000021216     D  PARAA2                20     23  0
006100021216     D  PARMG2                24     27  0
006200021219     D  PARME2                24     25  0
006300140530     D  PARgg2                26     27  0
006400021216     D  PARDF1                28     35  0
006500021216     D  PARDF2                36     43  0
006600021216     D  PARKS2                44     50  0
006700021216     D  PARASI                51     54  0
006800140530     D  PARlib                55     64
006900001205     C*------------------------------------------------------------------------*
007000001205     C* MAIN LINE
007100140529     d Wdata8          DS
007200140529     d  dadata                 1      8  0
007300140529     d  adata                  9     16  0
007400140529     d  GioLav                17     21  0
007500140530     c*
007600140530     d i_ann                               like(b_sdcann)
007700140530     d i_mes                               like(b_sdcmes)
007800140529     d i_irr           s                   like(b_sdcirr)
007900140529     d i_inc           s                   like(b_sdcinc)
008000140529     d i_irc           s                   like(b_sdcirc)
008100140529     d o_irr           s                   like(b_sdcirr)
008200140529     d o_inc           s                   like(b_sdcinc)
008300140529     d o_irc           s                   like(b_sdcirc)
008400140529     d ggme1           s              2  0
008500140529     d ggme2           s              2  0
008600140529     d ggperme1        s              2  0
008700140529     d ggperme2        s              2  0
008800140529     d dataiso         s               d
008900001205     C*------------------------------------------------------------------------*
009000001205     C*----
009100001205     C* SALDI CLIENTE TESTATA
009200001205     C*----
009300021219     C                   READ      SISDOU                                 99
009400001205DO  1C     *IN99         DOWEQ     *OFF
009500140530     c                   movel     u_sdoann      c_data            6 0
009600140530     c                   move      u_sdomes      c_data
009700140530     c                   exsr      srdata
009800140530IF  2C                   IF        dataui >= pards1 and
009900140529     C                             datauf <= pards2
010000021219IF  3C                   IF        U_SDOIRR <> 0 OR
010100021219     C                             U_SDOINC <> 0 OR
010200021219     C                             U_SDOIRC <> 0
010300021216     C                   Z-ADD     U_SDOKSC      KDOKSC
010400130228     C                   EVAL      KDOANN = parasi
010500021216     C                   Z-ADD     U_SDOMES      KDOMES
010600021216     C     KEYSDO        CHAIN     SISDOB                             98
010700140529     c                   if        (u_sdomes = parme1 and
010800140530     c                             u_sdoann = paraa1 and
010900140530     c                             ggme1 <> ggperme1) or
011000140529     c                             (u_sdomes = parme2 and
011100140530     c                             u_sdoann = paraa2 and
011200140530     c                             ggme2 <> ggperme2)
011300140529     c                   z-add     u_sdoann      i_ann
011400140529     c                   z-add     u_sdomes      i_mes
011500140529     C                   Z-ADD     u_SDoIRR      i_irr
011600140529     C                   Z-ADD     u_SDoINC      i_inc
011700140529     C                   Z-ADD     u_SDoIRC      i_irc
011800140529     c                   exsr      srripro
011900140529     C                   Z-ADD     o_irr         u_SDoIRR
012000140529     C                   Z-ADD     o_inc         u_SDoINC
012100140529     C                   Z-ADD     o_irc         u_SDoIRC
012200140529     c                   end
012300021219IF  4C     *IN98         IFEQ      *OFF
012400021219     C                   Z-ADD     U_SDOIRR      b_SDOIRR
012500021219     C                   Z-ADD     U_SDOINC      b_SDOINC
012600021219     C                   Z-ADD     U_SDOIRC      b_SDOIRC
012700001207     C                   UPDATE    SISDOBDG
012800021219X   4C                   ELSE
012900021219     C                   CLEAR                   SISDOBDG
013000021216     C                   EVAL      b_SDOANN = KDOANN
013100021216     C                   EVAL      b_SDOMES = KDOMES
013200021216     C                   EVAL      b_SDOKSC = KDOKSC
013300021216     C                   EVAL      b_SDOIRR = U_SDOIRR
013400021216     C                   EVAL      b_SDOINC = U_SDOINC
013500021216     C                   EVAL      b_SDOIRC = U_SDOIRC
013600021216     C                   WRITE     SISDOBDG
013700021219E   4C                   ENDIF
013800021219E   3C                   ENDIF
013900021219E   2C                   ENDIF
014000021219     C                   READ      SISDOU                                 99
014100001205E   1C                   ENDDO
014200001205     C*----
014300001205     C* SALDI CLIENTE DETTAGLIO
014400001205     C*----
014500021219     C                   READ      SISDCU                                 99
014600001205DO  1C     *IN99         DOWEQ     *OFF
014700140530     c                   movel     u_sdcann      c_data
014800140530     c                   move      u_sdcmes      c_data
014900140530     c                   exsr      srdata
015000140530IF  2C                   IF        dataui >= pards1 and
015100140530     C                             datauf <= pards2
015200021219IF  3C                   IF        U_SDCIRR <> 0 OR
015300021219     C                             U_SDCINC <> 0 OR
015400021219     C                             U_SDCIRC <> 0
015500021216     C                   Z-ADD     U_SDCKSC      KDCKSC
015600130228     C                   EVAL      KDCANN = parasi
015700021216     C                   Z-ADD     U_SDCMES      KDCMES
015800021216     C                   Z-ADD     U_SDCREP      KDCREP
015900021216     C                   Z-ADD     U_SDCREA      KDCREA
016000021216     C                   MOVEL     U_SDCFPE      KDCFPE
016100021216     C                   MOVEL     U_SDCTSP      KDCTSP
016200021216     C                   Z-ADD     U_SDCCTR      KDCCTR
016300021216     C                   MOVEL     U_SDCTPO      KDCTPO
016400021216     C                   MOVEL     U_SDCRBL      KDCRBL
016500021216     C                   MOVEL     U_SDCTPR      KDCTPR
016600021216     C     KEYSDC        CHAIN     SISDCB                             98
016700140529     c                   if        (u_sdcmes = parme1 and
016800140530     c                             u_sdcann = paraa1 and
016900140530     c                             ggme1 <> ggperme1) or
017000140529     c                             (u_sdcmes = parme2 and
017100140530     c                             u_sdcann = paraa2 and
017200140530     c                             ggme2 <> ggperme2)
017300140529     c                   z-add     u_sdcann      i_ann
017400140529     c                   z-add     u_sdcmes      i_mes
017500140529     C                   Z-ADD     u_SDCIRR      i_irr
017600140529     C                   Z-ADD     u_SDCINC      i_inc
017700140529     C                   Z-ADD     u_SDCIRC      i_irc
017800140529     c                   exsr      srripro
017900140529     C                   Z-ADD     o_irr         u_SDCIRR
018000140529     C                   Z-ADD     o_inc         u_SDCINC
018100140529     C                   Z-ADD     o_irc         u_SDCIRC
018200140529     c                   end
018300140529IF  4C     *IN98         IFEQ      *OFF
018400140529     C                   Z-ADD     U_SDCIRR      b_SDCIRR
018500140529     C                   Z-ADD     U_SDCINC      b_SDCINC
018600140529     C                   Z-ADD     U_SDCIRC      b_SDCIRC
018700140529     C                   UPDATE    SISDCBDG
018800140529X   4C                   ELSE
018900140529     C                   CLEAR                   SISDCBDG
019000021219     C                   EVAL      b_SDCANN = KDCANN
019100021219     C                   EVAL      b_SDCMES = KDCMES
019200021219     C                   EVAL      b_SDCKSC = KDCKSC
019300021219     C                   EVAL      b_SDCREP = KDCREP
019400021219     C                   EVAL      b_SDCREA = KDCREA
019500021219     C                   EVAL      b_SDCFPE = KDCFPE
019600021219     C                   EVAL      b_SDCTSP = KDCTSP
019700021219     C                   EVAL      b_SDCCTR = KDCCTR
019800021219     C                   EVAL      b_SDCTPO = KDCTPO
019900021219     C                   EVAL      b_SDCRBL = KDCRBL
020000021219     C                   EVAL      b_SDCTPR = KDCTPR
020100021219     C                   EVAL      b_SDCIRR = U_SDCIRR
020200021219     C                   EVAL      b_SDCINC = U_SDCINC
020300021219     C                   EVAL      b_SDCIRC = U_SDCIRC
020400021216     C                   WRITE     SISDCBDG
020500021219E   4C                   ENDIF
020600021219E   3C                   ENDIF
020700021219E   2C                   ENDIF
020800021219     C                   READ      SISDCU                                 99
020900001205E   1C                   ENDDO
021000001205     C*
021100001205     C                   SETON                                            LR
021200001205     C*------------------------------------------------------------------------*
021300140530     C* srdata  imposta data periodo del file saldi
021400001205     C*------------------------------------------------------------------------*
021500140530     C     srdata        BEGSR
021600140530     c* per prendere l'intero mese dei saldi devo impostare la data inizio
021700140530     c* con il gg di FINE mese, mentre la data fine con il gg di INIZIO mese
021800140530     c                   movel     c_data        dataui            8 0
021900140530     c                   movel     c_data        datauf            8 0
022000140530     c                   move      01            datauf
022100140530     c                   movel     datauf        dataiso
022200140530     c                   adddur    1:*m          dataiso
022300140530     c                   subdur    1:*d          dataiso
022400140530     c                   move      dataiso       dataui
022500140530     c                   endsr
022600140530     C*------------------------------------------------------------------------*
022700140530     C* srripro - riproporziono in base ai gg
022800140530     C*------------------------------------------------------------------------*
022900140530     C     srripro       BEGSR
023000140529     c                   clear                   o_irr
023100140529     c                   clear                   o_inc
023200140529     c                   clear                   o_irc
023300140529     c                   if        i_mes = parme1 and i_ann = paraa1 and
023400140529     c                             ggme1 <> 0 and ggperme1 <> 0
023500140529     c                   eval      o_irr = i_irr * ggperme1 / ggme1
023600140529     c                   eval      o_inc = i_inc * ggperme1 / ggme1
023700140529     c                   eval      o_irc = i_irc * ggperme1 / ggme1
023800140529     c                   end
023900140529     c                   if        i_mes = parme2 and i_ann = paraa2 and
024000140529     c                             ggme2 <> 0 and ggperme2 <> 0
024100140529     c                   eval      o_irr = i_irr * ggperme2 / ggme2
024200140529     c                   eval      o_inc = i_inc * ggperme2 / ggme2
024300140529     c                   eval      o_irc = i_irc * ggperme2 / ggme2
024400140529     c                   end
024500140529     c                   endsr
024600140528     C*------------------------------------------------------------------------*
024700140528     C* *INZSR - OPERAZIONI INIZIALI
024800140528     C*------------------------------------------------------------------------*
024900140528     C     *INZSR        BEGSR
025000021216     C*
025100021216     C* RICEVIMENTO PARAMETRI
025200021216     C     *ENTRY        PLIST
025300021216     C                   PARM                    KPJBA
025400021216     C                   MOVEL     KPJBU         PARAM
025500140528     c* verifico i gg lavorativi per poter riproporzionare le rettifiche
025600140528     c* dell'intero mese nei gg richiesti per la simulazione
025700140530     c*
025800140530     c                   if        pargg1 = 0
025900140530     c                   eval      pargg1 = 01
026000140530     c                   end
026100140530     c                   if        pargg2 = 0
026200140530     c                   eval      pargg2 = 01
026300140530     c                   move      pards2        dadata
026400140530     c                   movel     dadata        dataiso
026500140530     c                   adddur    1:*m          dataiso
026600140530     c                   subdur    1:*d          dataiso
026700140530     c                   move      dataiso       pards2
026800140530     c                   end
026900140528     C*
027000140528      *calcolo i gg lavorativi del mese di inizio
027100140530     c                   eval      dadata = pards1
027200140530     c                   move      01            dadata
027300140528     c                   movel     dadata        dataiso
027400140528     c                   adddur    1:*m          dataiso
027500140528     c                   subdur    1:*d          dataiso
027600140528     c                   move      dataiso       adata
027700140528     c                   CALL      'XSRLAV8'
027800140528     c                   PARM                    Wdata8
027900140528     c                   eval      ggme1 = giolav
028000140528      *calcolo i gg lavorativi del periodo
028100140528     c                   select
028200140528     c                   when      paraa1 = paraa2 and parme1 = parme2
028300140528     c                   eval      dadata = pards1
028400140528     c                   eval      adata = pards2
028500140528     c                   CALL      'XSRLAV8'
028600140528     c                   PARM                    Wdata8
028700140529     c                   eval      ggperme1 = giolav
028800140528     c                   when      parme1 <> parme2
028900140528      *calcolo i gg lavorativi del periodo per il mese di inizio
029000140528     c                   eval      dadata = pards1
029100140528     c                   movel     dadata        dataiso
029200140528     c                   adddur    1:*m          dataiso
029300140528     c                   subdur    1:*d          dataiso
029400140528     c                   move      dataiso       adata
029500140528     c                   CALL      'XSRLAV8'
029600140528     c                   PARM                    Wdata8
029700140528     c                   eval      ggperme1 = giolav
029800140528      *calcolo i gg lavorativi del mese di fine
029900140530     c                   eval      dadata = pards2
030000140530     c                   move      01            dadata
030100140528     c                   movel     dadata        dataiso
030200140528     c                   adddur    1:*m          dataiso
030300140528     c                   subdur    1:*d          dataiso
030400140528     c                   move      dataiso       adata
030500140528     c                   CALL      'XSRLAV8'
030600140528     c                   PARM                    Wdata8
030700140528     c                   eval      ggme2 = giolav
030800140528      *calcolo i gg lavorativi del periodo per il mese di fine
030900140530     c                   eval      dadata = pards2
031000140530     c                   move      01            dadata
031100140528     c                   eval      adata = pards2
031200140528     c                   CALL      'XSRLAV8'
031300140528     c                   PARM                    Wdata8
031400140528     c                   eval      ggperme2 = giolav
031500140528     c                   endsl
031600001205     C*
031700001205     C* CHIAVI DI LETTURA
031800001205     C     KEYSDO        KLIST
031900001205     C                   KFLD                    KDOKSC
032000001205     C                   KFLD                    KDOANN
032100001205     C                   KFLD                    KDOMES
032200001205     C     KEYSDC        KLIST
032300001205     C                   KFLD                    KDCKSC
032400001205     C                   KFLD                    KDCANN
032500001205     C                   KFLD                    KDCMES
032600001205     C                   KFLD                    KDCREP
032700001205     C                   KFLD                    KDCREA
032800001205     C                   KFLD                    KDCFPE
032900001205     C                   KFLD                    KDCTSP
033000001205     C                   KFLD                    KDCCTR
033100001205     C                   KFLD                    KDCTPO
033200001205     C                   KFLD                    KDCRBL
033300001205     C                   KFLD                    KDCTPR
033400001205     C*
033500001205     C                   ENDSR
