000100000612     H ALWNULL(*USRCTL)
000200000000     H*------------------------------------------------------------------------*
000300011120     H* VERIFICA LIVELLI DI GESTIONE CAP/LOC -Step2-
000400010904     F*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900140616     Fazcos00f  IF   E             DISK    rename(azcos000:azcosfff)
001000140616     Fazcos01l  UF   E           K DISK    PREFIX(u_)
001100011119     Fprtf198   O    f  198        PRINTER
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*------------------------------------------------------------------------*
001600000000     D* INPUT
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* ARCHITETTURA
002000000000     D*-------------------
002100000000     D KPJBA         E DS
002200011119     D*-------------------
002300011119     D* DS PER PASSAGGIO PARAMETRI A *PGM DI ELABORAZIONE
002400011119     D*-------------------
002500011119     D param           DS
002600011119     D  parver                        5S 0 inz(*zeros)
002700011119     D  parcap                        9    inz(*blanks)
002800011119     D  parsim                        1    inz('N')
002900000509     D*-------------------
003000000509     D* Variabili di lavoro
003100000509     D*-------------------
003200140616     D  w_key_rec      s             13s 0 INZ
003300140616     D  STPELA         s             15a   INZ(*blanks)
003400140616     D  wrktesto       s            198a   INZ(*blanks)
003500000000     C*------------------------------------------------------------------------*
003600000000     C* MAIN LINE
003700000000     C*------------------------------------------------------------------------*
003800140616     C*
003900140616     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004000140616     C
004100140616     C/EXEC SQL
004200140616     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004300140616     C/END-EXEC
004400140616     C*
004500011119     C                   EXSR      RUNMAIN
004600991118     C*
004700000000     C                   SETON                                        LR
004800000000     C*------------------------------------------------------------------------*
004900011119     C* RUNMAIN - ROUTINE DI ELABORAZIONE
005000000000     C*------------------------------------------------------------------------*
005100011119     C     RUNMAIN       BEGSR
005200140616     C*
005300140616     C/EXEC SQL
005400140616     C+ declare C1 cursor for
005500140616     C+ With SelTOT as (
005600140616     C+ SELECT COSVER, COSNAR, COSCAP, COSLNA, COSTSER, count(*) as TOT
005700140616     C+ FROM azcos00f WHERE cosver = :parver and cosloc <> ' '
005800140616     C+ GROUP BY COSVER, COSNAR, COSCAP, COSLNA, COSTSER),
005900140616     C+      SelORARI as (
006000140616     C+ SELECT COSVER, COSNAR, COSCAP, COSLNA, COSTSER,
006100140616     C+ COSOSTIS, COSOSTFS, COSOMIIS, COSOMXFS, COSOLRSC, COSOLRNC,
006200140616     C+ COSOLRNK, COSTRZS, COSTTC, COSSERV, COSFLO, count(*) as CNT_ORARI,
006300140616     C+ min(rrn(azcos00f)) as KEY_REC FROM azcos00f
006400140616     C+ WHERE cosver = :parver and cosloc <> ' ' GROUP BY COSVER, COSNAR,
006500140616     C+ COSCAP, COSLNA, COSTSER, COSOSTIS, COSOSTFS, COSOMIIS, COSOMXFS,
006600140616     C+ COSOLRSC, COSOLRNC, COSOLRNK, COSTRZS, COSTTC, COSSERV, COSFLO),
006700140616     C+      SelDATI as (
006800140616     C+ select t.COSVER, t.COSNAR, t.COSCAP, t.COSLNA, t.COSTSER, t.tot,
006900140616     C+ o.cnt_orari, o.key_rec, o.cnt_orari*100/t.tot as PERC
007000140616     C+ from SelTOT as t left join SelORARI as o
007100140616     C+ on t.COSVER=o.COSVER and t.COSNAR=o.COSNAR and t.COSCAP=o.COSCAP
007200140616     C+ and t.COSLNA=o.COSLNA and t.COSTSER=o.COSTSER)
007300140616     C+ select key_rec from SelDATI where PERC >= 70
007400140616     C+ for read only
007500140616     C/END-EXEC
007600140616     C
007700140616     C/EXEC SQL
007800140616     C+ open C1
007900140616     C/END-EXEC
008000140616     C
008100140616     C/EXEC SQL
008200140616     C+ Fetch next from C1 into :w_key_rec
008300140616     C/END-EXEC
008400140616     C*
008500140616     C                   dow       sqlcod = *zeros
008600140616     C*
008700140616     C* Per ogni orario di località con percentuale di "ripetività" >= 70%
008800140616     C* aggiorno il relativo orario per CAP
008900140616     C                   exsr      EXEOPE
009000140616     C*
009100140616     C/EXEC SQL
009200140616     C+ Fetch next from C1 into :w_key_rec
009300140616     C/END-EXEC
009400140616     C*
009500140616     C                   enddo
009600140616     C*
009700140616     C/EXEC SQL
009800140616     C+ close C1
009900140616     C/END-EXEC
010000140616     C*
010100991105     C*
010200000000     C                   ENDSR
010300010905     C*------------------------------------------------------------------------*
010400010905     C*  EXEOPE - ESEGUE L'EFFETTIVA ELABORAZIONE
010500010905     C*------------------------------------------------------------------------*
010600140616     C     EXEOPE        BEGSR
010700140616     C*
010800140616     C* 1° => chaino il record per RRN rilevato per reperire i valori da aggiornare
010900140616     C     w_key_rec     chain     azcos00f
011000140616     C                   if        %found(azcos00f)
011100140616     C*
011200140616     C* 2° => chaino il relativo record a livello di CAP ed aggiorno
011300140616     C                   eval      COSLOC  = *blanks                            * forzo blanks = CAP
011400140616     C     KEYcos01_C    chain     azcos01l
011500140616     C                   if        %found(azcos01l)
011600140616     C                   eval      u_COSOSTIS = COSOSTIS
011700140616     C                   eval      u_COSOSTFS = COSOSTFS
011800140616     C                   eval      u_COSOMIIS = COSOMIIS
011900140616     C                   eval      u_COSOMXFS = COSOMXFS
012000140616     C                   eval      u_COSOLRSC = COSOLRSC
012100140616     C                   eval      u_COSOLRNC = COSOLRNC
012200140616     C                   eval      u_COSOLRNK = COSOLRNK
012300140616     C                   eval      u_COSTRZS  = COSTRZS
012400140616     C                   eval      u_COSTTC   = COSTTC
012500140616     C                   eval      u_COSSERV  = COSSERV
012600140616     C                   eval      u_COSFLO   = COSFLO
012700140616     C                   eval      u_COSDUV   = %dec(%date() : *ISO)
012800140616     C                   eval      u_COSUTE   = 'EDPCED'
012900140616     C*
013000140616     C* Se NO simulazione => aggiorno
013100140616     C                   if        PARSIM='N'
013200140616     C                   update    azcos000
013300140616     C                   endif
013400140616     C*
013500140616     C* In ogni caso listo la modifica
013600140616     C                   exsr      EXESTAMPA
013700140616     C                   endif
013800140616     C*
013900140616     C                   endif
014000010905     C*
014100010905     C                   ENDSR
014200140616     C*------------------------------------------------------------------------*
014300140616     C*  EXESTAMPA - ESEGUE LE OPERAZIONI DI LISTING IN STAMPA
014400140616     C*------------------------------------------------------------------------*
014500140616     C     EXESTAMPA     BEGSR
014600011119     C*
014700011119     C* EFFETTUO OPERAZIONI DI AGGIORNAMENTO ARCHIVI E LISTING SE RICHIESTO NEL LANCIO
014800011120     C                   IF        PARSIM='N'
014900011120     C                   EVAL      STPELA = 'AGGIORNAMENTO'
015000011120     C                   ELSE
015100011120     C                   EVAL      STPELA = 'SIMULAZIONE'
015200011120     C                   ENDIF
015300140616     C                   EVAL      wrktesto = STPELA + ' Aggiornati valori' +
015400140616     C                             ' orari da località a CAP => ' +
015500140616     C                             ' Vers: '       + %editc(COSVER:'X') +
015600140616     C                             ' Naz: '        + COSNAR  +
015700140616     C                             ' CAP: '        + COSCAP  +
015800140616     C                             ' LNA: '        + %editc(COSLNA:'X') +
015900140616     C                             ' TipSrv: '     + COSTSER
016000011119     C                   EXCEPT    d_listing
016100010905     C*
016200010905     C                   ENDSR
016300000000     C*------------------------------------------------------------------------*
016400980403     C* *INZSR - OPERAZIONI INIZIALI
016500000000     C*------------------------------------------------------------------------*
016600980403     C     *INZSR        BEGSR
016700000000     C*
016800000000     C* RICEVIMENTO PARAMETRI
016900000000     C     *ENTRY        PLIST
017000000000     C                   PARM                    KPJBA
017100011119     C*
017200011119     C                   MOVEL     kpjbu         param
017300140616     C*
017400140616     C* AZCOS01L - Completa
017500140616     C     KEYcos01_C    KLIST
017600140616     C                   KFLD                    COSVER
017700140616     C                   KFLD                    COSNAR
017800140616     C                   KFLD                    COSCAP
017900140616     C                   KFLD                    COSLOC
018000140616     C                   KFLD                    COSLNA
018100140616     C                   KFLD                    COSTSER
018200000000     C*
018300000000     C                   ENDSR
018400011119
018500011119     Oprtf198   E            t_listing         2
018600011119     O                                              'Controllo LIVELLI GESTIONE'
018700011119     O          E            d_listing   1
018800011119     O                       wrktesto
