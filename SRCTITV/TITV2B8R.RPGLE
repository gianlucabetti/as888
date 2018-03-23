000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400110112     Ftitas30c  IF   E           K DISK
000500110113     Ffiar401l  IF   E           K DISK    usropn
000600110113     F                                     extfile(LibFileAR4)
000700080924     Ftivgdtmp  uF   E             DISK
000800090326     Ffnvacwwt  O    E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300080924     D fnvacds       e ds                  extname(fnvac00t)
001400110113     D LibFileAR4      s             21A   inz
001500110113     D currSysNeta     s              8A   inz('*NULL')
001600110113
001700110113
001800110113     D*-------------------
001900941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002000110113     D*-------------------
002100941221     D* 10+3
002200941221     D                 DS
002300941221     D  WS15                   1      1
002400941221     D  WI15                   2     11
002500991125     D  WV15                  12     12    inz(',')
002600941221     D  WD15                  13     15
002700941221     D  WASC15                 1     15
002800941221     D*  6+1
002900941221     D                 DS
003000941221     D  WS09                   1      1
003100941221     D  WI09                   2      7
003200991125     D  WV09                   8      8    inz(',')
003300941221     D  WD09                   9      9
003400941221     D  WASC09                 1      9
003500941221     D*  2+3
003600941221     D                 DS
003700941221     D  WS07                   1      1
003800941221     D  WI07                   2      3
003900991125     D  WV07                   4      4    inz(',')
004000941221     D  WD07                   5      7
004100941221     D  WASC07                 1      7
004200021007     D*-------------------
004300021007     D* COSTANTI
004400021007     D*-------------------
004500021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
004600021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004700021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004800021007     D*-------------------
004900021007     D* SCHIERE DI WRK
005000021007     D*-------------------
005100021007     D SKcharOK        S              1    DIM(92)
005200081017
005300081017
005400081017     D*------------------
005500081017     D* LINKING A DEFINIZIONI ESTERNE
005600081017     D*------------------
005700081017     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005800110112     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005900110113     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006000110113     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006100110113
006200021007
006300021007
006400110113     C*
006500110113     C* Reperisco il sistema AS/400 corrente
006600110113     C                   callp     UBRTVNETA_Rtv(currSysNeta)
006700110113     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
006800110113     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
006900110113     C                   else
007000110113     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
007100110113     C                   endif
007200110113     C*
007300110113     C                   if        not %open(FIAR401L)
007400110113     C                   open      FIAR401L
007500110113     C                   endif
007600110113     C*
007700021008     C                   MOVEA     charOK        SKcharOK
007800991027     C                   EXSR      traduci
007900110113     C*
008000110113     C                   if        %open(FIAR401L)
008100110113     C                   close     FIAR401L
008200110113     C                   endif
008300110113     C*
008400921023     C                   SETON                                        LR
008500991027
008600081017
008700081017
008800991027     C     traduci       BEGSR
008900991027     C*
009000080924     C                   READ      tivgdtmp
009100050622     C                   DOW       not %eof
009200110112     C                   movel     vgddta        fnvacds
009300110113     C*
009400110113     C* Inizializzo i campi d wrk
009500110113     C                   movel     *blanks       wEsito1           1
009600110113     C                   movel     *blanks       wEsito2           1
009700110113     C                   movel     *blanks       wBolTyp           1
009800110113     C                   movel     *blanks       wBolOrm           1
009900110113     C*
010000110113     C* Salvo subito la DCM "originale" del record corrente
010100110113     C                   z-add     VACDCM        wDCM              8 0
010200110113     C*
010300110113     C* Verifico subito se trattasi d bolla originale o meno
010400110113     C*
010500110113     C* Chiamata metodo GetLblTyp
010600110113     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
010700110113     C                                                VACAAS
010800110113     C                                               :VACLNP
010900110113     C                                               :VACNRS
011000110113     C                                               :VACNSP
011100110113     C                                               :pOutLblTyp
011200110113     C                                               :pOutAnnoBO
011300110113     C                                               :pOutLineaParBO
011400110113     C                                               :pOutSerieBO
011500110113     C                                               :pOutNumSpedBO
011600110113     C                                               :pOutDcmBO
011700110113     C                                               :pOutCcaBO
011800110113     C                                               :pOutRblBO))
011900110113     C*
012000110113     C* Se bolla originale me lo segno e reperisco l'ultima bolla figlia
012100110113     C                   if        pOutLblTyp = 'O'
012200110113     C                   eval      wBolTyp = 'O'
012300110113     C*
012400110113     C* Chiamata metodo GetLastChild
012500110113     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
012600110113     C                                                VACAAS
012700110113     C                                               :VACLNP
012800110113     C                                               :VACNRS
012900110113     C                                               :VACNSP
013000110113     C                                               :pOutAnnoFI
013100110113     C                                               :pOutLineaParFI
013200110113     C                                               :pOutSerieFI
013300110113     C                                               :pOutNumSpedFI
013400110113     C                                               :pOutDcmFI
013500110113     C                                               :pOutCcaFI))
013600110113     C                   endif
013700110112     C*
013800110113     C* Verifico origine dati
013900110112     C                   select
014000110112     C                   when      vgdPGM = 'FNLSA9R'
014100110112     C*
014200110112     C* Verifico se trattasi d bolla "da ORM"
014300110112     C                   eval      ar4TRC = 'M'
014400110112     C     KEYar401_C    chain     fiar401l
014500110112     C                   if        %found(fiar401l)
014600110112     C                   eval      wBolOrm = 'S'
014700110112     C                   endif
014800110112     C*
014900110112     C* Effettuo verifiche x ritornare al cliente solamente:
015000110113     C*   - bolle "ritorno" su reso destinatario "da ORM"
015100110112     C*   - resi su mancata consegna bolle "andata"
015200110112     C*
015300110112     C                   select
015400110112     C* ...se bolla "ritorno" su reso destinatario "da ORM"
015500110112     C                   when      VACNRS=0 and wBolTyp='O' and wBolOrm='S'
015600110112     C*
015700110112     C* ====>
015800110112     C                   select
015900110113     C                   when      VACDCM>*zeros and VACCCA='C'
016000110112     C                   eval      VACCCA = 'N'
016100110113     C* Aggancio la bolla originale in sede
016200110113     C                   eval      tasAAS = vacAAS
016300110113     C                   eval      tasLNP = vacLNP
016400110113     C                   eval      tasNRS = vacNRS
016500110113     C                   eval      tasNSP = vacNSP
016600110112     C     KEYtas30_C    chain     titas30c
016700110112     C                   if        %found(titas30c)
016800110112     C                   eval      VACDCM = tasAAS*10000+tasMGS
016900110112     C                   endif
017000110113     C                   exsr      wriRec
017100110112     C*
017200110112     C                   when      VACDCM>*zeros and VACCCA=' '
017300110112     C                   eval      VACCCA = '2'
017400110113     C                   exsr      wriRec
017500110112     C                   endsl
017600110112     C* <====
017700110112     C*
017800110112     C* ...resi su mancata consegna bolle "andata"
017900110113     C                   when      VACNRS>0 and VACCCA='2' and wBolTyp='O'
018000110113     C*
018100110113     C                   select
018200110113     C* ...se trattasi d chiusura bolla originale con reso
018300110113     C                   when      VACDCM <> pOutDcmFI
018400110113     C                   eval      VACCCA = 'N'
018500110113     C                   exsr      wriRec
018600110113     C*
018700110113     C* ...se trattasi d chiusura ok bolla di reso
018800110113     C                   when      VACDCM = pOutDcmFI and pOutCcaFI = '2'
018900110113     C                   eval      VACCCA = '2'
019000110113     C                   exsr      wriRec
019100110113     C                   endsl
019200110113     C*
019300110113     C                   endsl
019400110113     C*
019500110113     C                   when      vgdPGM = 'TIS7VCR'
019600110113     C*
019700110113     C* Effettuo verifiche x ritornare al cliente solamente:
019800110113     C*   - bolle "ritorno" su reso destinatario "nn da ORM"
019900110113     C                   select
020000110113     C                   when      VACNRS=0 and wBolTyp='O' and wBolOrm<>'S'
020100110113     C*
020200110113     C* Aggancio la bolla originale in sede
020300110113     C                   eval      tasAAS = vacAAS
020400110113     C                   eval      tasLNP = vacLNP
020500110113     C                   eval      tasNRS = vacNRS
020600110113     C                   eval      tasNSP = vacNSP
020700110113     C     KEYtas30_C    chain     titas30c
020800110113     C                   if        %found(titas30c)
020900110113     C                   if        tasCCM <> tasKSC
021000110113     C*
021100110113     C* ...scrivo il primo esito relativo alla "nascita" bolle "ritorno" su reso destinatario
021200110113     C                   eval      VACCCA = 'N'
021300110113     C                   eval      VACDCM = tasAAS*10000+tasMGS
021400110113     C                   exsr      wriRec
021500110113     C*
021600110113     C* ...scrivo il secondo esito relativo "consegna" bolle "ritorno" su reso destinatario
021700110113     C                   eval      VACCCA = '2'
021800110113     C                   eval      VACDCM = wDCM
021900110113     C                   exsr      wriRec
022000110113     C*
022100110113     C                   endif
022200110113     C                   endif
022300110113     C*
022400110113     C                   endsl
022500110112     C*
022600110112     C                   endsl
022700080613     C*
022800080924     c                   delete    tivgd000
022900991027     C*
023000080924     C                   READ      tivgdtmp
023100991027     C                   ENDdo
023200991027     C*
023300991027     C                   eval      wrkesito = '0'
023400991027     C*
023500910830     C                   ENDSR
023600110113
023700110113
023800110113
023900110113     C     wriRec        BEGSR
024000110113     C*
024100110113     C                   MOVE      VACAAS        VHCAAS
024200110113     C                   MOVE      VACLNP        VHCLNP
024300110113     C                   MOVE      VACNRS        VHCNRS
024400110113     C                   MOVE      VACNSP        VHCNSP
024500110113     C                   MOVE      VACMGS        VHCMGS
024600110113     C                   MOVE      VACCBO        VHCCBO
024700110113     C                   MOVE      VACLNA        VHCLNA
024800110113     C                   MOVE      VACRSD        VHCRSD
024900110113     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
025000110113     C     1             DO        35            x                 2 0
025100110113     C                   MOVEL     *blanks       wrkCHAR           1
025200110113     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
025300110113     C                   IF        wrkCHAR <> *blanks AND
025400110113     C                             wrkCHAR <> ''''
025500110113     C     wrkCHAR       LOOKUP    SKcharOK                               20
025600110113     C                   IF        *in20 = *off
025700110113     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
025800110113     C                   ENDIF
025900110113     C                   ENDIF
026000110113     C                   ENDDO
026100110113     C*
026200110113     C                   MOVE      VACPRD        VHCPRD
026300110113     C                   MOVE      VACGC1        VHCGC1
026400110113     C                   MOVE      VACGC2        VHCGC2
026500110113     C                   MOVE      VACCTR        VHCCTR
026600110113     C                   MOVE      VACCTS        VHCCTS
026700110113     C                   MOVE      VACFTM        VHCFTM
026800110113     C                   MOVE      VACFIN        VHCFIN
026900110113     C                   MOVE      VACFAP        VHCFAP
027000110113     C                   MOVE      VACTSP        VHCTSP
027100110113     C*
027200110113     C                   MOVE      VACIAS        WD15
027300110113     C                   MOVEL     VACIAS        WI15
027400110113     C                   MOVE      WASC15        VHCIAS
027500110113     C*
027600110113     C                   MOVE      VACVAS        VHCVAS
027700110113     C                   MOVE      VACNAS        VHCNAS
027800110113     C                   MOVE      VACNCL        VHCNCL
027900110113     C*
028000110113     C                   MOVE      VACPKB        WD09
028100110113     C                   MOVEL     VACPKB        WI09
028200110113     C                   MOVE      WASC09        VHCPKB
028300110113     C*
028400110113     C                   MOVE      VACVLB        WD07
028500110113     C                   MOVEL     VACVLB        WI07
028600110113     C                   MOVE      WASC07        VHCVLB
028700110113     C*
028800110113     C                   MOVE      VACQFT        WD15
028900110113     C                   MOVEL     VACQFT        WI15
029000110113     C                   MOVE      WASC15        VHCQFT
029100110113     C*
029200110113     C                   MOVE      VACTIC        VHCTIC
029300110113     C*
029400110113     C                   MOVE      VACCAS        WD15
029500110113     C                   MOVEL     VACCAS        WI15
029600110113     C                   MOVE      WASC15        VHCCAS
029700110113     C*
029800110113     C                   MOVE      VACVCA        VHCVCA
029900110113     C                   MOVE      VACCCM        VHCCCM
030000110113     C                   MOVE      VACRMN        VHCRMN
030100110113     C                   MOVE      VACRMA        VHCRMA
030200110113     C                   MOVE      VACRMO        VHCRMO
030300110113     C                   MOVE      VACFFD        VHCFFD
030400110113     C                   MOVE      VACTCR        VHCTCR
030500110113     C                   MOVE      VACDCR        VHCDCR
030600110113     C                   MOVE      VACHCR        VHCHCR
030700110113     C                   MOVE      VACDCM        VHCDCM
030800110113     C                   MOVE      VACHMC        VHCHMC
030900110113     C                   MOVE      VACTC1        VHCTC1
031000110113     C                   MOVE      VACTC2        VHCTC2
031100110113     C                   MOVE      VACCCA        VHCCCA
031200110113     C                   MOVE      VACDLA        VHCDLA
031300110113     C                   MOVE      VACDAG        VHCDAG
031400110113     C*
031500110113     C                   WRITE     FNVACAA
031600110113     C*
031700110113     C                   ENDSR
031800991027
031900081017
032000081017
032100991027      /TITLE Operazioni iniziali.
032200991027     C     *inzsr        BEGSR
032300991027     C*
032400991027     C     *ENTRY        PLIST
032500991027     C                   parm                    prmppt
032600991027     C     wrkesito      parm      wrkesito      prmesito
032700110112     C*
032800110112     C* DEFINIZIONE CHIAVI
032900110112     C*
033000110112     C* Chiave su TITAS30C - Completa
033100110112     C     KEYtas30_C    klist
033200110112     C                   kfld                    tasAAS
033300110112     C                   kfld                    tasLNP
033400110112     C                   kfld                    tasNRS
033500110112     C                   kfld                    tasNSP
033600110112     C*
033700110112     C* Chiave su FIAR401L - Completa
033800110112     C     KEYar401_C    klist
033900110112     C                   kfld                    vacAAS
034000110112     C                   kfld                    vacLNP
034100110112     C                   kfld                    vacNRS
034200110112     C                   kfld                    vacNSP
034300110112     C                   kfld                    ar4TRC
034400110112     C*
034500991027     C                   ENDSR
