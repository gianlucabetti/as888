000100160616      /TITLE Upload via Internet: traduzioni da TELLUS
000200160616     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500010330     D*----------------------------------------------------
000600070124     D* DICHIARAZIONE VARIABILI DI WRK
000700010330     D*----------------------------------------------------
000800070124
000900010330     D psds           sds
001000010330     D  procname         *PROC
001100010330     D tivlrds       e ds                  extname(tivlr00f)
001200160616     D dtllxi        e ds
001300010330     D esito           s              1
001400160616     D wEnvironment    s              1    inz
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800060906     D*------------------
001900160616
002000160616
002100160616
002200080109     C*
002300990921     C                   reset                   esito
002400990921     C                   reset                   wrkesito
002500010601     C*
002600010601     C                   exsr      opeini
002700060803     C                   exsr      rwfile
002800010601     C*
002900060905     C                   seton                                        LR
003000010601     C***
003100160616
003200160616
003300010330     C*--------------------------------------------------------
003400060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
003500010330     C*--------------------------------------------------------
003600060803     C     RWFILE        BEGSR
003700010330     C*
003800010330     C                   if        not %open(tivin00r)
003900010330     C                   open      tivin00r
004000010330     C                   endif
004100010330     C*
004200160616     C                   clear                   低TROK            7 0
004300160616     C                   clear                   低TRMO            7 0
004400160616     C                   clear                   低TRNO            7 0
004500060720     C*
004600160616     C                   exsr      DEFCAM
004700160616     C*
004800160616     C                   DO        *HIVAL
004900160616     C*
005000160616     C                   READ      tivin00r                               70
005100160616     C*
005200160616     C* Dopo ogni lettura verifico se ci sono stati record OK
005300160616     C                   if        vinflg = '1'
005400160616     C                   eval      flgOk = '1'
005500160616     C                   endif
005600160616     C*
005700160616     C                   if        vindta > *blanks
005800160616     C*
005900160616     C                   if        *in70 = *off and
006000160616     C                             (vinflg = *blanks
006100160616     C                              or vinflg = '0'
006200160616     C                              or vinflg = '2')
006300010330     C*
006400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
006500160616     C                   if        vinflg = *blanks or vinflg = '0'
006600010711     C                   clear                   vinmsg
006700160616     C                   endif
006800010601     C*
006900060803     C* Esegue l'effettiva acquisizione dei dati
007000160616     C                   exsr      IMPFILE
007100010601     C*
007200010604     C* Ebbene...
007300160616     C* *IN31 errore bloccante sul record
007400010604     C*
007500160616     C                   if        not *in31
007600160616     C                   add       1             低TROK
007700160616     C                   else
007800160616     C                   add       1             低TRNO
007900160616     C                   endif
008000020722     C*
008100160616     C                   endif
008200160616     C*
008300160616     C                   if        *in31 = *off
008400010604     C                   eval      vinflg = '1'
008500010604     C                   else
008600010604     C                   eval      vinflg = '2'
008700160616     C                   endif
008800010604     C*
008900010330     C                   else
009000010330     C                   eval      vinflg = '1'
009100160616     C                   endif
009200010601     C*
009300010601     C  N70              update    tivin000
009400010330     C*
009500160616     C  N70              ENDdo
009600010601     C
009700010601     C*
009800010330     C* Se non ci sono record con errori ...
009900010601     C                   if        呃trno = 0 and
010000060803     C                             呃trmo = 0
010100010330     C* ... restituisco esito OK.
010200010330     C                   eval      wrkesito = '0'
010300010330     C                   else
010400010330     C                   if        呃trok > 0
010500010330     C                   eval      wrkesito = '1'
010600010330     C                   else
010700010615     C                   if        flgOk = '0'
010800010615     C                   eval      wrkesito = '2'
010900010615     C                   else
011000010615     C                   eval      wrkesito = '6'
011100010615     C                   endif
011200010330     C                   endif
011300010330     C                   endif
011400010330     C*
011500010330     C                   if        %open(tivin00r)
011600010330     C                   close     tivin00r
011700010330     C                   endif
011800160616     C*
011900010330     C                   ENDSR
012000010330     C***
012100010601
012200160616
012300010330     C*----------------------------------------------------*
012400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
012500010330     C*----------------------------------------------------*
012600010330     C     DEFCAM        BEGSR
012700010330     C*
012800160616     C* Inizializzi
012900160616     C                   CLEAR                   DTLLXI
013000160616     C                   EVAL      wEnvironment = 'P'
013100160616     C*
013200020204     C* Imposto i valori di default...
013300020204     C* ... e poi verifico se sono stati passati come parametri
013400020204     C                   IF        vlrppt > *blanks
013500160616     C                   EVAL      wEnvironment = %subst(vlrppt:1:1)
013600020204     C                   ENDIF
013700020204     C*
013800010330     C                   ENDSR
013900010607     C*----------------------------------------------------*
014000160512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNORG)
014100010607     C*----------------------------------------------------*
014200160616     C     IMPFILE       BEGSR
014300160616     C*
014400160616     C                   SETOFF                                       31
014500160616     C*
014600160616     C                   MOVEL     *blanks       wETYP             3
014700160616     C                   EVAL      wETYP = %subst(vindta:1:3)
014800160616     C*
014900160616     C* A seconda del tipo elemento (sempre nei primi 3 bytes) ...
015000160616     C* ... chiamo il *pgm appropriato
015100160616     C                   SELECT
015200160616     C*
015300160616     C                   WHEN      wETYP = 'V01'
015400160616     C                   EVAL      DTLLXI = vindta
015500160617     C                   CALL(e)   'FIORG12R'
015600160616     C                   PARM                    wEnvironment
015700160616     C                   PARM                    DTLLXI
015800160616     C                   PARM                    wError            1 0
015900160616     C                   IF        %error OR wError <> *zeros
016000160616     C                   SETON                                        31
016100160616     C                   ENDIF
016200160616     C*
016300160616     C                   ENDSL
016400020204     C*
016500160616     C                   ENDSR
016600070604
016700010601
016800060906     C*----------------------------------------------------*
016900160616     C/TITLE Operazioni iniziali
017000060906     C*----------------------------------------------------*
017100010601     C     opeini        BEGSR
017200010601     C*
017300010601     C* Inizializzo flag e contatori operativi
017400010615     C                   movel     '0'           flgOk             1
017500010601     C*
017600010601     C                   ENDSR
017700010601     C***
017800010330
017900160616
018000060906     C*----------------------------------------------------*
018100000613     C     *inzsr        BEGSR
018200060906     C*----------------------------------------------------*
018300990910     C*
018400990910     C     *entry        plist
018500990920     C                   parm                    tivlrds
018600990921     C                   parm      wrkesito      esito
018700000724     C                   parm                    prmlit
018800000710     C                   parm                    prmfir
018900000613     C*
019000000613     C                   ENDSR
