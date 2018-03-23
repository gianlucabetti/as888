000100070531      /TITLE Upload via Internet: traduzione in FIGRG00F
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500070531     FFIGRG01L  uF   E           K DISK
000600070531     FFIGRV02L  iF   E           K DISK
000700010330     D*----------------------------------------------------
000800070124     D* DICHIARAZIONE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000070124
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400070531     D dtllgi        e ds
001500070709     D dgrgflo       e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100011113     D depcmd          s            150    INZ(*blanks)
002200070531     D wCurrRec        s              6  0 INZ(*zeros)
002300070531     D wFlgOk          s              1  0 INZ(*zeros)
002400010330     D*------------------
002500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600010330     D*------------------
002700010330     D WLBDA8          DS                  INZ
002800010330     D  G08DAT                 1      8  0
002900010330     D  G08INV                 9     16  0
003000010330     D  G08ERR                17     17
003100010330     D  G08TGI                18     22  0
003200060906     D*------------------
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800060803     C                   exsr      rwfile
003900010601     C*
004000060905     C                   seton                                        LR
004100010601     C***
004200010330     C*--------------------------------------------------------
004300060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
004400010330     C*--------------------------------------------------------
004500060803     C     RWFILE        BEGSR
004600010330     C*
004700010330     C                   if        not %open(tivin00r)
004800010330     C                   open      tivin00r
004900010330     C                   endif
005000010330     C*
005100070531     C                   clear                   §CTROK            7 0
005200070531     C                   clear                   §CTRMO            7 0
005300070531     C                   clear                   §CTRNO            7 0
005400060720     C*
005500060720     C                   exsr      defcam
005600010330     C*
005700010330     C                   DO        *HIVAL
005800010330     C*
005900010330     C                   READ      tivin00r                               70
006000070531     C                   add       1             wCurrRec
006100010618     C*
006200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
006300010618     C                   if        vinflg = '1'
006400010618     C                   eval      flgOk = '1'
006500010618     C                   endif
006600010618     C*
006700010330     C                   if        vindta > *blanks
006800010330     C                   add       1             rrnum
006900010330     C*
007000010601     C                   if        *in70 = *off and
007100010330     C                             (vinflg = *blanks
007200010330     C                              or vinflg = '0'
007300010330     C                              or vinflg = '2')
007400010330     C*
007500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
007600010711     C                   if        vinflg = *blanks or vinflg = '0'
007700010711     C                   clear                   vinmsg
007800010711     C                   endif
007900010601     C*
008000060803     C* Esegue l'effettiva acquisizione dei dati
008100060803     C                   exsr      impfile
008200070531     C
008300070124     C                   endif
008400020722     C*
008500070531     C                   if        *in31 = *off
008600010604     C                   eval      vinflg = '1'
008700010604     C                   else
008800010604     C                   eval      vinflg = '2'
008900010604     C                   endif
009000010604     C*
009100010330     C                   else
009200010330     C                   eval      vinflg = '1'
009300010330     C                   endif
009400010601     C*
009500010601     C  N70              update    tivin000
009600010330     C*
009700010330     C  N70              ENDdo
009800070614     C*
009900070614     C* Al termine scorro nuovamente tutti i giri x verificare quelli annullati/scaduti
010000070614     C                   if        wFlgOk = 1
010100070614     C     KEYgrg01_p1   reade     figrg01l
010200070614     C                   dow       not %eof(figrg01l)
010300070709     c                   eval      dgrgflo = grgflo
010400070709      * solo per i giri consegna
010500070709     c                   If        §grgtug = 'C' or §grgtug = 'E'
010600070709
010700070614     C                   if        grgDSCAD <  datcor or
010800070614     C                             grgATB   <> *blanks
010900070614     C                   if        grgACT = *blanks
011000070614     C                   eval      grgFTT = *blanks
011100070614     C                   endif
011200070614     C                   endif
011300070709
011400070709     c                   endif
011500070614     C                   update    figrg000
011600070614     C     KEYgrg01_p1   reade     figrg01l
011700070614     C                   enddo
011800070614     C                   endif
011900010601     C
012000010601     C*
012100010330     C* Se non ci sono record con errori ...
012200010601     C                   if        §ctrno = 0 and
012300060803     C                             §ctrmo = 0
012400010330     C* ... restituisco esito OK.
012500010330     C                   eval      wrkesito = '0'
012600010330     C                   else
012700010330     C                   if        §ctrok > 0
012800010330     C                   eval      wrkesito = '1'
012900010330     C                   else
013000010615     C                   if        flgOk = '0'
013100010615     C                   eval      wrkesito = '2'
013200010615     C                   else
013300010615     C                   eval      wrkesito = '6'
013400010615     C                   endif
013500010330     C                   endif
013600010330     C                   endif
013700010330     C*
013800010330     C                   if        %open(tivin00r)
013900010330     C                   close     tivin00r
014000010330     C                   endif
014100070531     C*
014200010330     C                   ENDSR
014300010330     C***
014400010601
014500010601
014600010330     C*----------------------------------------------------*
014700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
014800010330     C*----------------------------------------------------*
014900010330     C     DEFCAM        BEGSR
015000020204     C*
015100010330     C                   ENDSR
015200010607     C*----------------------------------------------------*
015300060803     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIARG)
015400010607     C*----------------------------------------------------*
015500070112     C     IMPFILE       BEGSR
015600010607     C*
015700070531     C                   SETOFF                                       31
015800060720     C*
015900060804     C* Ridefinisco i dati ricevuti da TELLUS
016000070531     C                   EVAL      dtllgi = vindta
016100070531     C*
016200070531     C* Al 1° record chaino innanzitutto la versione
016300070531     C                   if        wCurrRec = 1
016400070531     C                   move(p)   §TLLGIFGS     grvFGS
016500070531     C                   movel(p)  §TLLGIVER     grvKEY
016600070531     C     KEYgrv02_c    chain     figrv02l
016700070531     C                   if        %found(figrv02l)
016800070531     C     KEYgrg01_p1   setll     figrg01l
016900070531     C                   if        %equal(figrg01l)
017000070531     C                   eval      wFlgOk = 1
017100070531     C*
017200070531     C* 1° giro => sfleggo tutti i giri della filiale/versione
017300070531     C     KEYgrg01_p1   reade     figrg01l
017400070531     C                   dow       not %eof(figrg01l)
017500070709     c                   eval      dgrgflo = grgflo
017600070709      * solo per i giri consegna
017700070709     c                   If        §grgtug = 'C' or §grgtug = 'E'
017800070709
017900070531     C                   eval      grgACT = *blanks
018000070709     c                   endif
018100070531     C                   update    figrg000
018200070531     C     KEYgrg01_p1   reade     figrg01l
018300070531     C                   enddo
018400070531     C                   endif
018500070531     C                   endif
018600070531     C                   endif
018700070531     C*
018800070531     C* 2° giro => se tutto ok fleggo i giri effettivamente attivi della filiale/versione
018900070531     C                   if        wFlgOk = 1
019000070531     C     KEYgrg01_c    chain     figrg01l
019100070531     C                   if        %found(figrg01l)
019200070709     c                   eval      dgrgflo = grgflo
019300070709      * solo per i giri consegna
019400070709     c                   If        §grgtug = 'C' or §grgtug = 'E'
019500070531     C                   eval      grgACT = 'S'
019600070709     c                   endif
019700070531     C                   update    figrg000
019800070531     C                   endif
019900070531     C*
020000070531     C                   else
020100070531     c                   seton                                        31
020200070531     C                   eval      vinmsg = 'Versione giri non trovata'
020300070531     C                   endif
020400020204     C*
020500070531     C                   ENDSR
020600070531
020700070531
020800070531
020900060906     C*----------------------------------------------------*
021000060803      /TITLE Operazioni iniziali
021100060906     C*----------------------------------------------------*
021200010601     C     opeini        BEGSR
021300010601     C*
021400010601     C* Inizializzo flag e contatori operativi
021500010601     C                   movel     '0'           flgGiro           1
021600010615     C                   movel     '0'           flgOk             1
021700010601     C*
021800010601     C                   ENDSR
021900010601     C***
022000010330
022100060906     C*----------------------------------------------------*
022200000613     C     *inzsr        BEGSR
022300060906     C*----------------------------------------------------*
022400990910     C*
022500990910     C     *entry        plist
022600990920     C                   parm                    tivlrds
022700990921     C                   parm      wrkesito      esito
022800000724     C                   parm                    prmlit
022900000710     C                   parm                    prmfir
023000010330     C*
023100010330     C* CALCOLA LA DATA CORRENTE
023200010330     C                   time                    wn14             14 0
023300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
023400010330     C                   z-add     wn8           g08dat
023500010330     C                   z-add     *zeros        g08inv
023600010330     C                   movel     '0'           g08err
023700010330     C                   call      'XSRDA8'
023800010330     C                   parm                    wlbda8
023900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
024000060807     C*
024100060807     C* Definizione chiavi
024200060807     C*
024300070531     C* Chiave su FIGRV02L - Completa
024400070531     C     KEYgrv02_c    KLIST
024500070531     C                   KFLD                    grvFGS
024600070531     C                   KFLD                    grvKEY
024700070531     C*
024800070531     C* Chiave su FIGRG01L - Completa
024900070531     C     KEYgrg01_c    KLIST
025000070531     C                   KFLD                    grvDDE
025100070531     C                   KFLD                    grvFGS
025200070531     C                   KFLD                    §TLLGICGI
025300070531     C*
025400070531     C* Chiave su FIGRG01L - Parziale 1
025500070531     C     KEYgrg01_p1   KLIST
025600070531     C                   KFLD                    grvDDE
025700070531     C                   KFLD                    grvFGS
025800000613     C*
025900000613     C                   ENDSR
