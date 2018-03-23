000100020405     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200001006
000300990317      ****************************************************************
000400070611     FFIMS51D   CF   E             WORKSTN
000500061122     f                                     infds(INFDSPF)
000600080117     FFIPSL00f  IF   E             DISK
000700070611     Ftabel00f  IF   E           K DISK
000800980521      *------------------------------------------------------------------------*
000900080117     D*KFIL            S                   LIKE(pslfil)
001000080117     D*KTIM            S                   like(psldtorar)
001100080117     D*KMIL            S                   LIKE(pslmilles)
001200080117     D*Kiddisp         S                   LIKE(psliddisp)
001300070611     D cod             S                   LIKE(tblcod)
001400070611     D key             S                   LIKE(tblkey)
001500070611     D codut           S                   LIKE(tblkut) inz(1)
001600020405
001700980521     D KPJBA         E DS
001800061114      * - Parametri x Controllo profilo utenti
001900061114     d TIBS34DS      e ds                  inz
002000061114      * - Ds di riferimento al file esterno AZUTE00F
002100061114     d AZUTEDS       e ds                  extname(AZUTE00F)
002200061114      * - Ds per dati organigramma
002300061114     d DDatiUte      e ds
002400061114
002500061114     d Status         sds
002600061114     d  SDSpgm           *proc
002700061122      *-------------
002800061122     d InfDspf         ds           512
002900061122     d  ID_NrRg              370    370
003000061122     d  ID_NrCl              371    371
003100061122      *-------------
003200061122      * posizione cursore
003300061122     d Cursor          ds                  inz
003400061122     d  Csr_RiRi               1      2b 0 inz
003500061122     d  Csr_Rg                 2      2
003600061122     d  Csr_CoCo               3      4b 0 inz
003700061122     d  Csr_Cl                 4      4
003800061122      *
003900061122     d Cur_R           s              2  0 inz
004000061122     d Cur_C           s              2  0 inz
004100010604
004200010607     D Parm01          DS
004300080117     D**pfil                          3
004400080117     D**pdat                          8  0
004500080117     D**pora                          6  0
004600080117     D**pmil                          3
004700080117     D**piddisp                      35
004800080117     D  pnrr                          7S 0
004900020405
005000020405     D WLBDAT          DS                  INZ
005100020405     D  G02DAT                 1      8  0
005200020405     D  G02INV                 9     16  0
005300020405     D  G02ERR                17     17
005400020405     D  G02TGI                18     22  0
005500010531      *****************************************************************
005600010531     c                   exsr      gesvid
005700010531     c     inizio        tag
005800010607     c                   write     fimst01                                      intestazione
005900010607     c                   exfmt     fimsd01                                      descrizioni
006000010607     c   kl              return
006100061122     c                   if        *inKA
006200070611     c*                  exsr      sr_Help
006300061122     c                   endif
006400010531     c                   goto      inizio
006500010531      *****************************************************************
006600010531      * gestione caricamento videata
006700010531      *****************************************************************
006800010531     C     gesvid        BEGSR
006900010531      *                                                                     ---*
007000010607     c                   movel     kpjbu         parm01
007100061114     c                   movel     SDSpgm        T01pgm
007200061122     c*** NO x usr EDP*: eval      *in40 = (%subst(KNMUS:1:3) = 'EDP')
007300020405
007400080117     c**                 movel     pfil          KFIL
007500080117     c**                 movel     pdat          KTIM
007600080117     c**                 move      pora          KTIM
007700080117     c**                 move      pmil          KMIL
007800080117     c**                 move      piddisp       Kiddisp
007900080117     c     pnrr          chain     fipsl00f
008000080117     c                   if        %found(fipsl00f)
008100020405
008200070611     c                   movel     pslprfc       v1cprfc
008300070611     c                   movel     psliddisp     v1ciddisp
008400071005     c* profilo di connessione
008500071005     c                   eval      v1cindip=%subst(pslindip:1:3)+'.'+
008600071005     c                             %subst(pslindip:4:3)+'.'+
008700071005     c                             %subst(pslindip:7:3)+'.'+
008800071005     c                             %subst(pslindip:10:3)+'.'
008900070611     c                   move      pslnpg        v1cnpg
009000070611     c                   move      pslnfv        v1cnfv
009100070611     c                   move      pslbarco      v1cbar
009200070611     c                   move      pslcan        v1canm
009300070611     c* decodifica cod.anomalia
009400070611     c                   clear                   v1danm
009500070611     c                   movel     '3E'          cod
009600070611     c                   movel(p)  pslcan        key
009700070611     c     ktab          chain     tabel00f
009800070611     c                   if        %found(tabel00f)
009900070611     c                   movel     tbluni        v1danm
010000070611     c                   endif
010100070611     c                   move      psldup        v1cdup
010200070611     c                   move      pslman        v1cman
010300070611     c                   move      pslusr        v1cusr
010400070611     c* inversione data rilevazione
010500070611     c                   clear                   wlbdat
010600070611     c                   movel     psldtorar     g02inv
010700070611     c                   movel     '3'           g02err
010800070611     c                   call      'XSRDA8'
010900070611     C                   parm                    wlbdat
011000070611     c                   movel     G02dat        v1cdtas
011100070611     c                   move      psldtorar     v1coras
011200070611     c                   move      pslmilles     v1cmis
011300070611     c* inversione data ricezione
011400070611     c                   clear                   wlbdat
011500070611     c                   movel     psldatora     g02inv
011600070611     c                   movel     '3'           g02err
011700070611     c                   call      'XSRDA8'
011800070611     C                   parm                    wlbdat
011900070611     c                   movel     G02dat        v1cdtar
012000070611     c                   move      psldatora     v1corar
012100070611     c                   if        pslerr = *blanks
012200070611     C                   eval      V1Derr = 'SPUNTA VALIDA    '
012300070611     c                   else
012400070611     C                   eval      V1Derr = 'SPUNTA NON VALIDA'
012500080117     c                   select
012600080117     c                   when      pslerr='I'
012700071016     c                   eval      v1derr=%trim(v1derr) + '-Errore ind.IP'
012800080117     c                   when      pslerr='L'
012900080117     c                   eval      v1derr=%trim(v1derr) + '-Errore linea '
013000080117     c                   endsl
013100071016     c                   endif
013200070611     c***
013300070611     C                   MOVEL     pslfil        V1Cfil
013400020405
013500020405
013600020405     c                   endif
013700010531      *                                                                     ---*
013800010531     c                   endsr
013900010531      *****************************************************************
014000010531      * ROUTINE INIZIALE
014100010531      *****************************************************************
014200010531     C     *INZSR        BEGSR
014300010531      *                                                                     ---*
014400010531     C     *ENTRY        PLIST
014500010531     C                   PARM                    KPJBA
014600010531      *
014700061114     c     *dtaara       define    §azute        azuteds
014800061114     c     *dtaara       define    §datiute      ddatiute
014900061114      *
015000061114     c                   in(E)     *dtaara
015100061114     c                   IF        %ERROR or RSUT = *blanks
015200061114     c                   clear                   Tibs34Ds
015300061114     c                   call      'TIBS34R'
015400061114     c                   parm                    Tibs34Ds
015500061114     c                   in        *dtaara
015600061114     c                   ENDIF
015700020405
015800080117     C*    KFipsl        KLIST
015900080117     C*                  KFLD                    KFIL
016000080117     C*                  KFLD                    KTIM
016100080117     C*                  KFLD                    KMIL
016200080117     C*                  KFLD                    KIDDisp
016300070611     c     kTAB          klist
016400070611     c                   kfld                    codut
016500070611     c                   kfld                    cod
016600070611     c                   kfld                    key
016700010531     c                   endsr
016800010531      *****************************************************************
016900070611     c*    sr_HELP       BEGSR
017000061122      *
017100061122      * Determina Riga/Colonna del cursore
017200070611     c*                  move      ID_NrRg       Csr_Rg
017300070611     c*                  move      ID_NrCl       Csr_Cl
017400070611     c*                  z-add     Csr_RiRi      Cur_R
017500070611     c*                  z-add     Csr_CoCo      Cur_C
017600061122      *
017700070611sel 1c*                  SELECT
017800061122      *  4ª riga
017900070611w   1c*                  WHEN      Cur_R  = 4
018000070611sel 2c*                  select
018100070611w   2c*                  when      Cur_C <= 23
018200070611     c*                  exfmt     fimshCTL
018300070611w   2c*                  when      Cur_C <= 53
018400070611     c*                  exfmt     fimshNFV
018500070611x   2c*                  other
018600070611     c*                  exfmt     fimshNPL
018700070611e   2c*                  endsl
018800061122      *  6ª riga
018900070611w   1c*                  WHEN      Cur_R  = 6
019000070611sel 2c*                  select
019100070611w   2c*                  when      Cur_C <= 20
019200070611     c*                  exfmt     fimshPIC
019300070611w   2c*                  other
019400070611     c*                  exfmt     fimshTIM
019500070611e   2c*                  endsl
019600061122      *  8ª riga
019700070611w   1c*                  WHEN      Cur_R  = 8
019800070611     c*                  exfmt     fimsh020
019900061122      * 10ª riga
020000070611w   1c*                  WHEN      Cur_R  = 10
020100070611sel 2c*                  select
020200070611w   2c*                  when      Cur_C <= 32
020300070611     c*                  exfmt     fimshPOD
020400070611w   2c*                  when      Cur_C <= 47
020500070611     c*                  exfmt     fimshODS
020600070611w   2c*                  when      Cur_C <= 65
020700070611     c*                  exfmt     fimshPAD
020800070611x   2c*                  other
020900070611     c*                  exfmt     fimshDTS
021000070611e   2c*                  endsl
021100061122      * 12ª riga
021200070611w   1c*                  WHEN      Cur_R  = 12
021300070611sel 2c*                  select
021400070611w   2c*                  when      Cur_C <= 31
021500070611     c*                  exfmt     fimshPEP
021600070611w   2c*                  when      Cur_C <= 40
021700070611     c*                  exfmt     fimshPES
021800070611w   2c*                  when      Cur_C <= 70
021900070611     c*                  exfmt     fimshPUP
022000070611w   2c*                  other
022100070611     c*                  exfmt     fimshPUS
022200070611e   2c*                  endsl
022300061122      * 15ª riga
022400070611w   1c*                  WHEN      Cur_R  = 15
022500070611sel 2c*                  select
022600070611w   2c*                  when      Cur_C <= 28
022700070611     c*                  exfmt     fimshPS1
022800070611w   2c*                  when      Cur_C <= 40
022900070611     c*                  exfmt     fimshPN1
023000070611w   2c*                  other
023100070611     c*                  exfmt     fimshPD1
023200070611e   2c*                  endsl
023300061122      * 16ª riga
023400070611w   1c*                  WHEN      Cur_R  = 16
023500070611sel 2c*                  select
023600070611w   2c*                  when      Cur_C <= 28
023700070611     c*                  exfmt     fimshPS2
023800070611w   2c*                  when      Cur_C <= 40
023900070611     c*                  exfmt     fimshPN2
024000070611w   2c*                  other
024100070611     c*                  exfmt     fimshPD2
024200070611e   2c*                  endsl
024300061122      * 17ª riga
024400070611w   1c*                  WHEN      Cur_R  = 17
024500070611sel 2c*                  select
024600070611w   2c*                  when      Cur_C <= 28
024700070611     c*                  exfmt     fimshPS3
024800070611w   2c*                  when      Cur_C <= 40
024900070611     c*                  exfmt     fimshPN3
025000070611w   2c*                  other
025100070611     c*                  exfmt     fimshPD3
025200070611e   2c*                  endsl
025300061122      * 18ª riga
025400070611w   1c*                  WHEN      Cur_R  = 18
025500070611sel 2c*                  select
025600070611w   2c*                  when      Cur_C <= 28
025700070611     c*                  exfmt     fimshPS4
025800070611w   2c*                  when      Cur_C <= 40
025900070611     c*                  exfmt     fimshPN4
026000070611w   2c*                  other
026100070611     c*                  exfmt     fimshPD4
026200070611e   2c*                  endsl
026300061122      * 20ª riga
026400061122w   1c***                WHEN      Cur_R  = 20
026500061122     c***                exfmt     fimshCRC
026600061122      * 22ª riga
026700070611w   1c*                  WHEN      Cur_R  = 22
026800070611sel 2c*                  select
026900070611w   2c*                  when      Cur_C <= 18
027000070611     c*                  exfmt     fimshPGR
027100070611w   2c*                  when      Cur_C <= 35
027200070611     c*                  exfmt     fimshVOL
027300070611w   2c*                  when      Cur_C <= 63
027400070611     c*                  exfmt     fimshBAR
027500070611w   2c*                  other
027600070611     c*                  exfmt     fimshFIL
027700070611e   2c*                  endsl
027800061122      *
027900070611e   1c*                  ENDSL
028000061122      *
028100061122      * Reimposta pos. del cursore
028200070611     c*                  z-add     Cur_R         H1Riga
028300070611     c*                  z-add     Cur_C         H1Colo
028400061122      *
028500070611     c*                  ENDSR
