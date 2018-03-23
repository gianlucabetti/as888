000100130530     /*PRM  dbgview(*source)
000200130530     /*END
000300020405     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000400001006
000500990317      ****************************************************************
000600010531     FFIMS41D   CF   E             WORKSTN
000700061122     f                                     infds(INFDSPF)
000800130530     FFLTR702L  IF   E           K DISK
000900040603     FTNTBE01L  IF   E           K DISK
001000980521      *------------------------------------------------------------------------*
001100040603     d MTRk            S              2    DIM(50) INZ                          *entrance point
001200040603     d MTRd            S             50    DIM(50) INZ                           -n° entrance point
001300020405
001400020405     D KFIL            S                   LIKE(TR7FIL)
001500020405     D KTIM            S                   LIKE(TR7TIM)
001600020405     D KMIL            S                   LIKE(TR7MIL)
001700130529     d kPD1            s                   like(TR7pd1) inz
001800130529     d kPD2            s                   like(TR7pd2) inz
001900040603     D Kcod            S                   LIKE(TBECOD) INZ('MTR')
002000040603     D xx              S              3  0
002100020405
002200980521     D KPJBA         E DS
002300061114      * - Parametri x Controllo profilo utenti
002400061114     d TIBS34DS      e ds                  inz
002500061114      * - Ds di riferimento al file esterno AZUTE00F
002600061114     d AZUTEDS       e ds                  extname(AZUTE00F)
002700061114      * - Ds per dati organigramma
002800061114     d DDatiUte      e ds
002900061114
003000061114     d Status         sds
003100061114     d  SDSpgm           *proc
003200061122      *-------------
003300061122     d InfDspf         ds           512
003400061122     d  ID_NrRg              370    370
003500061122     d  ID_NrCl              371    371
003600061122      *-------------
003700061122      * posizione cursore
003800061122     d Cursor          ds                  inz
003900061122     d  Csr_RiRi               1      2b 0 inz
004000061122     d  Csr_Rg                 2      2
004100061122     d  Csr_CoCo               3      4b 0 inz
004200061122     d  Csr_Cl                 4      4
004300061122      *
004400061122     d Cur_R           s              2  0 inz
004500061122     d Cur_C           s              2  0 inz
004600010604
004700010607     D Parm01          DS
004800020405     D  pfil                          3
004900020405     D  pdat                          8  0
005000020405     D  pora                          6  0
005100130529     D  pmil                               like(TR7mil)
005200130529     d  Ppd1                               like(TR7pd1)
005300130529     d  Ppd2                               like(TR7pd2)
005400020405
005500020405     D WLBDAT          DS                  INZ
005600020405     D  G02DAT                 1      8  0
005700020405     D  G02INV                 9     16  0
005800020405     D  G02ERR                17     17
005900020405     D  G02TGI                18     22  0
006000010531      *****************************************************************
006100010531     c                   exsr      gesvid
006200010531     c     inizio        tag
006300010607     c                   write     fimst01                                      intestazione
006400010607     c                   exfmt     fimsd01                                      descrizioni
006500010607     c   kl              return
006600061122     c                   if        *inKA
006700061122     c***                          and *in40
006800061122     c                   exsr      sr_Help
006900061122     c                   endif
007000010531     c                   goto      inizio
007100010531      *****************************************************************
007200010531      * gestione caricamento videata
007300010531      *****************************************************************
007400010531     C     gesvid        BEGSR
007500010531      *                                                                     ---*
007600010607     c                   movel     kpjbu         parm01
007700061114     c                   movel     SDSpgm        T01pgm
007800061122     c*** NO x usr EDP*: eval      *in40 = (%subst(KNMUS:1:3) = 'EDP')
007900020405
008000020405     c                   movel     pfil          KFIL
008100020405     c                   movel     pdat          KTIM
008200020405     c                   move      pora          KTIM
008300020405     c                   move      pmil          KMIL
008400130529     c                   eval      kPD1 = pPD1
008500130529     c                   eval      kPD2 = pPD2
008600130530     c     kfltr7        chain     fltr7000
008700130530     c                   if        %found(fltr702l)
008800020405
008900020405     C                   MOVEL     tr7CTL        V1CCTL
009000020405     C                   MOVEL     tr7NPL        V1CNPL
009100020405     C                   MOVEL     tr7NFV        V1CNFV
009200020405     C                   MOVEL     tr7tim        V1CDAT
009300020405
009400020405     c                   z-add     v1cdat        g02inv
009500020405     c                   movel     '3'           g02err
009600020405     c                   call      'XSRDA8'
009700020405     C                   parm                    wlbdat
009800020405     c                   movel     G02dat        v1cdat
009900020405
010000020405     C                   MOVE      tr7tim        V1CTIM
010100020405     C                   MOVEL     tr7MIL        V1CMIL
010200040211     C**                 MOVEL     tr7COD        V1CCOD
010300020405     C                   MOVEL     tr7020        V1C020
010400040603     c                   if        V1C020 = *blanks
010500040603     C                   eval      V1D020 = 'TIPO RECORD NON VALIDO'
010600040603     c                   else
010700040603     c                   z-add     1             xx
010800040603     c     v1c020        lookup    MTRk(XX)                               99
010900040603     C  N99              eval      V1D020 = 'TIPO RECORD NON VALIDO'
011000040603     C   99              eval      V1D020 = MTRd(XX)
011100040603     c                   endif
011200020405     C                   MOVEL     tr7PIC        V1CPIC
011300071126     C***                MOVEL     tr7PGR        V1CPGR
011400071126     c                   if             TR7020 <> '22'
011500071126     c                             and  TR7020 <> '2C'
011600071126     c                             and  TR7020 <> '2K'
011700071126      * - Non RPVP (peso in grammi)
011800071126     c                   if        %subst(tr7PGR:1:1) >= '0'
011900071126     c                   eval      V1Cpgr = '0' + tr7PGR
012000071126     c                   else
012100071126     c                   eval      V1Cpgr = ' ' + tr7PGR
012200071126     c                   endif
012300071126     c                   else
012400071126      * - RPVP (peso in decagrammi)
012500071126     c                   eval      V1Cpgr = tr7PGR + '0'
012600071126     c                   endif
012700020405     C                   MOVEL     tr7POD        V1CPOD
012800020405     C                   MOVEL     tr7ODS        V1CODS
012900020405     C                   MOVEL     tr7PAD        V1CPAD
013000020405     C                   MOVEL     tr7DTS        V1CDTS
013100020405     C                   MOVEL     tr7PS1        V1CPS1
013200020405     C                   MOVEL     tr7PN1        V1CPN1
013300020405     C                   MOVEL     tr7PD1        V1CPD1
013400020405     C                   MOVEL     tr7PS2        V1CPS2
013500020405     C                   MOVEL     tr7PN2        V1CPN2
013600020405     C                   MOVEL     tr7PD2        V1CPD2
013700020405     C                   MOVEL     tr7PS3        V1CPS3
013800020405     C                   MOVEL     tr7PN3        V1CPN3
013900020405     C                   MOVEL     tr7PD3        V1CPD3
014000020405     C                   MOVEL     tr7PS4        V1CPS4
014100020405     C                   MOVEL     tr7PN4        V1CPN4
014200020405     C                   MOVEL     tr7PD4        V1CPD4
014300020405     C                   MOVEL     tr7PEP        V1CPEP
014400020405     C                   MOVEL     tr7PES        V1CPES
014500020405     C                   MOVEL     tr7PUP        V1CPUP
014600020405     C                   MOVEL     tr7PUS        V1CPUS
014700020405     c                   if        tr7CRC = *blanks
014800010621     C                   eval      V1DCRC = 'SPUNTA VALIDA    '
014900010620     c                   else
015000010621     C                   eval      V1DCRC = 'SPUNTA NON VALIDA'
015100010620     c                   endif
015200020405     C                   MOVEL     tr7VOL        V1CVOL
015300020405     C                   MOVEL     tr7BAR        V1CBAR
015400020405     C                   MOVEL     tr7FIL        V1CFIL
015500020405
015600020405     c                   endif
015700010531      *                                                                     ---*
015800010531     c                   endsr
015900010531      *****************************************************************
016000010531      * ROUTINE INIZIALE
016100010531      *****************************************************************
016200010531     C     *INZSR        BEGSR
016300010531      *                                                                     ---*
016400010531     C     *ENTRY        PLIST
016500010531     C                   PARM                    KPJBA
016600010531      *
016700061114     c     *dtaara       define    §azute        azuteds
016800061114     c     *dtaara       define    §datiute      ddatiute
016900061114      *
017000061114     c                   in(E)     *dtaara
017100061114     c                   IF        %ERROR or RSUT = *blanks
017200061114     c                   clear                   Tibs34Ds
017300061114     c                   call      'TIBS34R'
017400061114     c                   parm                    Tibs34Ds
017500061114     c                   in        *dtaara
017600061114     c                   ENDIF
017700020405
017800020405     C     KFLTR7        KLIST
017900020405     C                   KFLD                    KFIL
018000020405     C                   KFLD                    KTIM
018100130529     C                   KFLD                    KMIL
018200130529     c                   kfld                    kPD1
018300130529     c                   kfld                    kPD2
018400040603
018500040603     C     KTBE          KLIST
018600040603     C                   KFLD                    Kcod
018700010608
018800040603     c                   clear                   xx
018900040603     C     KTBE          SETLL     tntbe000
019000040603     C     KTBE          READE     tntbe000
019100040603     c                   DOW       NOT %EOF(tntbe01L)
019200040603     c                   add       1             xx
019300040603     c                   movel     TBEKE1        MTRk(xx)
019400040603     c                   movel     TBEUNI        MTRd(xx)
019500040603     C     KTBE          READE     tntbe000
019600040603     c                   ENDDO
019700040603
019800010531     c                   endsr
019900010531      *****************************************************************
020000061122     c     sr_HELP       BEGSR
020100061122      *
020200061122      * Determina Riga/Colonna del cursore
020300061122     c                   move      ID_NrRg       Csr_Rg
020400061122     c                   move      ID_NrCl       Csr_Cl
020500061122     c                   z-add     Csr_RiRi      Cur_R
020600061122     c                   z-add     Csr_CoCo      Cur_C
020700061122      *
020800061122sel 1c                   SELECT
020900061122      *  4ª riga
021000061122w   1c                   WHEN      Cur_R  = 4
021100061122sel 2c                   select
021200061122w   2c                   when      Cur_C <= 23
021300061122     c                   exfmt     fimshCTL
021400061122w   2c                   when      Cur_C <= 53
021500061122     c                   exfmt     fimshNFV
021600061122x   2c                   other
021700061122     c                   exfmt     fimshNPL
021800061122e   2c                   endsl
021900061122      *  6ª riga
022000061122w   1c                   WHEN      Cur_R  = 6
022100061122sel 2c                   select
022200061122w   2c                   when      Cur_C <= 20
022300061122     c                   exfmt     fimshPIC
022400061122w   2c                   other
022500061122     c                   exfmt     fimshTIM
022600061122e   2c                   endsl
022700061122      *  8ª riga
022800061122w   1c                   WHEN      Cur_R  = 8
022900061122     c                   exfmt     fimsh020
023000061122      * 10ª riga
023100061122w   1c                   WHEN      Cur_R  = 10
023200061122sel 2c                   select
023300061122w   2c                   when      Cur_C <= 32
023400061122     c                   exfmt     fimshPOD
023500061122w   2c                   when      Cur_C <= 47
023600061122     c                   exfmt     fimshODS
023700061122w   2c                   when      Cur_C <= 65
023800061122     c                   exfmt     fimshPAD
023900061122x   2c                   other
024000061122     c                   exfmt     fimshDTS
024100061122e   2c                   endsl
024200061122      * 12ª riga
024300061122w   1c                   WHEN      Cur_R  = 12
024400061122sel 2c                   select
024500061122w   2c                   when      Cur_C <= 31
024600061122     c                   exfmt     fimshPEP
024700061122w   2c                   when      Cur_C <= 40
024800061122     c                   exfmt     fimshPES
024900061122w   2c                   when      Cur_C <= 70
025000061122     c                   exfmt     fimshPUP
025100061122w   2c                   other
025200061122     c                   exfmt     fimshPUS
025300061122e   2c                   endsl
025400061122      * 15ª riga
025500061122w   1c                   WHEN      Cur_R  = 15
025600061122sel 2c                   select
025700061122w   2c                   when      Cur_C <= 28
025800061122     c                   exfmt     fimshPS1
025900061122w   2c                   when      Cur_C <= 40
026000061122     c                   exfmt     fimshPN1
026100061122w   2c                   other
026200061122     c                   exfmt     fimshPD1
026300061122e   2c                   endsl
026400061122      * 16ª riga
026500061122w   1c                   WHEN      Cur_R  = 16
026600061122sel 2c                   select
026700061122w   2c                   when      Cur_C <= 28
026800061122     c                   exfmt     fimshPS2
026900061122w   2c                   when      Cur_C <= 40
027000061122     c                   exfmt     fimshPN2
027100061122w   2c                   other
027200061122     c                   exfmt     fimshPD2
027300061122e   2c                   endsl
027400061122      * 17ª riga
027500061122w   1c                   WHEN      Cur_R  = 17
027600061122sel 2c                   select
027700061122w   2c                   when      Cur_C <= 28
027800061122     c                   exfmt     fimshPS3
027900061122w   2c                   when      Cur_C <= 40
028000061122     c                   exfmt     fimshPN3
028100061122w   2c                   other
028200061122     c                   exfmt     fimshPD3
028300061122e   2c                   endsl
028400061122      * 18ª riga
028500061122w   1c                   WHEN      Cur_R  = 18
028600061122sel 2c                   select
028700061122w   2c                   when      Cur_C <= 28
028800061122     c                   exfmt     fimshPS4
028900061122w   2c                   when      Cur_C <= 40
029000061122     c                   exfmt     fimshPN4
029100061122w   2c                   other
029200061122     c                   exfmt     fimshPD4
029300061122e   2c                   endsl
029400061122      * 20ª riga
029500061122w   1c***                WHEN      Cur_R  = 20
029600061122     c***                exfmt     fimshCRC
029700061122      * 22ª riga
029800061122w   1c                   WHEN      Cur_R  = 22
029900061122sel 2c                   select
030000061122w   2c                   when      Cur_C <= 18
030100061122     c                   exfmt     fimshPGR
030200061122w   2c                   when      Cur_C <= 35
030300061122     c                   exfmt     fimshVOL
030400061122w   2c                   when      Cur_C <= 63
030500061122     c                   exfmt     fimshBAR
030600061122w   2c                   other
030700061122     c                   exfmt     fimshFIL
030800061122e   2c                   endsl
030900061122      *
031000061122e   1c                   ENDSL
031100061122      *
031200061122      * Reimposta pos. del cursore
031300061122     c                   z-add     Cur_R         H1Riga
031400061122     c                   z-add     Cur_C         H1Colo
031500061122      *
031600061122     c                   ENDSR
