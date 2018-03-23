000100060803      /TITLE Upload via Internet: traduzione in FIARG00F
000200990908     H dftactgrp(*yes)
000300000313     F*
000400110713     Ftivin00r  UF   E             DISK    usropn
000500110713     Fwftell0f  UF A E             DISK
000600010330     D*----------------------------------------------------
000700070124     D* DICHIARAZIONE VARIABILI DI WRK
000800010330     D*----------------------------------------------------
000900070124
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300060804     D dtllsi        e ds
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D wrkdata         s               d
001900010330     D wrkora          s               t
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100060906     D*------------------
002200110713     C*
002300990915     C                   time                    wrkdata
002400990915     C                   time                    wrkora
002500000913     C                   reset                   rrnum
002600990921     C                   reset                   esito
002700990921     C                   reset                   wrkesito
002800010601     C*
002900010601     C                   exsr      opeini
003000060803     C                   exsr      rwfile
003100010601     C*
003200060905     C                   seton                                        LR
003300010601     C***
003400010330     C*--------------------------------------------------------
003500060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
003600010330     C*--------------------------------------------------------
003700060803     C     RWFILE        BEGSR
003800010330     C*
003900010330     C                   if        not %open(tivin00r)
004000010330     C                   open      tivin00r
004100010330     C                   endif
004200010330     C*
004300110713     C                   clear                   §CTROK            7 0
004400110713     C                   clear                   §CTRMO            7 0
004500110713     C                   clear                   §CTRNO            7 0
004600060720     C*
004700060720     C                   exsr      defcam
004800010330     C*
004900080109    1C                   DO        *HIVAL
005000010330     C*
005100010330     C                   READ      tivin00r                               70
005200010618     C*
005300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
005400080109    2C                   if        vinflg = '1'
005500010618     C                   eval      flgOk = '1'
005600080109    2C                   endif
005700010618     C*
005800080109    2C                   if        vindta > *blanks
005900010330     C                   add       1             rrnum
006000010330     C*
006100080109    3C                   if        *in70 = *off and
006200010330     C                             (vinflg = *blanks
006300010330     C                              or vinflg = '0'
006400010330     C                              or vinflg = '2')
006500010330     C*
006600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
006700080109    4C                   if        vinflg = *blanks or vinflg = '0'
006800010711     C                   clear                   vinmsg
006900080109    4C                   endif
007000010601     C*
007100060803     C* Esegue l'effettiva acquisizione dei dati
007200060803     C                   exsr      impfile
007300010604     C*
007400080109    3C                   endif
007500020722     C*
007600080109    3C                   if        *in31 = *off and *in33 =*off   and
007700010604     C                             *in32 = *off
007800010604     C                   eval      vinflg = '1'
007900010604     C                   else
008000010604     C                   eval      vinflg = '2'
008100080109    3C                   endif
008200010604     C*
008300010330     C                   else
008400010330     C                   eval      vinflg = '1'
008500080109    2C                   endif
008600010601     C*
008700010601     C  N70              update    tivin000
008800010330     C*
008900080109    1C  N70              ENDdo
009000010601     C
009100010601     C*
009200010330     C* Se non ci sono record con errori ...
009300010601     C                   if        §ctrno = 0 and
009400060803     C                             §ctrmo = 0
009500010330     C* ... restituisco esito OK.
009600010330     C                   eval      wrkesito = '0'
009700010330     C                   else
009800010330     C                   if        §ctrok > 0
009900010330     C                   eval      wrkesito = '1'
010000010330     C                   else
010100010615     C                   if        flgOk = '0'
010200010615     C                   eval      wrkesito = '2'
010300010615     C                   else
010400010615     C                   eval      wrkesito = '6'
010500010615     C                   endif
010600010330     C                   endif
010700010330     C                   endif
010800010330     C*
010900010330     C                   if        %open(tivin00r)
011000010330     C                   close     tivin00r
011100010330     C                   endif
011200010330     C*
011300010330     C                   ENDSR
011400010330     C***
011500010601
011600010601
011700010330     C*----------------------------------------------------*
011800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
011900010330     C*----------------------------------------------------*
012000010330     C     DEFCAM        BEGSR
012100010330     C*
012200110713     C                   clear                   WFTELL
012300110713     C*
012400110713     C* ... e poi verifico se sono stati passati parametri
012500020204     C                   IF        vlrppt > *blanks
012600020204     C                   ENDIF
012700020204     C*
012800010330     C                   ENDSR
012900010607     C*----------------------------------------------------*
013000060803     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIARG)
013100010607     C*----------------------------------------------------*
013200070112     C     IMPFILE       BEGSR
013300010607     C*
013400060905     C                   SETOFF                                       313233
013500060720     C*
013600060804     C* Ridefinisco i dati ricevuti da TELLUS
013700060804     C                   EVAL      dtllsi = vindta
013800110713     C*
013900110713     C                   eval      WFTEDATI = vindta
014000110713     C                   eval      WFTESPE  = %subst(vindta:1:16)
014100110713     C                   eval      WFTECGI  = §TLLSICGI
014200110713     C                   eval      WFTELNO  = §TLLSILNO
014300110714     C*
014400110714     C                   select
014500110714     C                   when      WFTELNO  = 'V'
014600110714     C                   eval      WFTELNON = 10
014700110714     C                   when      WFTELNO  = 'VA'
014800110714     C                   eval      WFTELNON = 9
014900110714     C                   when      WFTELNO  = 'L'
015000110714     C                   eval      WFTELNON = 8
015100110714     C                   when      WFTELNO  = 'C'
015200110714     C                   eval      WFTELNON = 7
015300110714     C                   when      WFTELNO  = 'LA'
015400110714     C                   eval      WFTELNON = 6
015500110714     C                   when      WFTELNO  = 'CA'
015600110714     C                   eval      WFTELNON = 5
015700110714     C                   when      WFTELNO  = 'P'
015800110714     C                   eval      WFTELNON = 2
015900110714     C                   other
016000110715     C                   eval      WFTELNON = 1
016100110714     C                   endsl
016200110714     C*
016300110713     C                   eval      WFTETIP  = %subst(vlrppt:1:3)
016400110713     C                   eval      WFTEDAT  = datoracor
016500110713     C*
016600110713     C                   write     WFTELL
016700020204     C*
016800110713     C                   ENDSR
016900110713     C***
017000070604
017100010601
017200060906     C*----------------------------------------------------*
017300110713     C/TITLE Operazioni iniziali
017400060906     C*----------------------------------------------------*
017500010601     C     opeini        BEGSR
017600010601     C*
017700010601     C* Inizializzo flag e contatori operativi
017800010601     C                   movel     '0'           flgGiro           1
017900010615     C                   movel     '0'           flgOk             1
018000010601     C*
018100010601     C                   ENDSR
018200010601     C***
018300110713
018400110713
018500060906     C*----------------------------------------------------*
018600000613     C     *inzsr        BEGSR
018700060906     C*----------------------------------------------------*
018800990910     C*
018900990910     C     *entry        plist
019000990920     C                   parm                    tivlrds
019100990921     C                   parm      wrkesito      esito
019200000724     C                   parm                    prmlit
019300000710     C                   parm                    prmfir
019400010330     C*
019500010330     C* CALCOLA LA DATA CORRENTE
019600110713     C                   time                    wn14             14 0
019700110713     C                   z-add     *zeros        datcor            8 0          *DATA     CORRENTE
019800110713     C                   movel     wn14          oracor            6 0          *ORA      CORRENTE
019900110713     C                   z-add     *zeros        datoracor        14 0          *ORA      CORRENTE
020000110713     C                   eval      datcor    = %dec(%date() : *ISO)
020100110713     C                   eval      datoracor = datcor*1000000+oracor
020200000613     C*
020300000613     C                   ENDSR
