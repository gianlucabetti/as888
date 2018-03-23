000100101025      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500171024     FEDIVAB7I  uF   E           K DISK    usropn
000600171024     F                                     extfile(LibFileVAB7)
000700171024     FEDIVAT3L  uF   E           K DISK    usropn
000800171024     F                                     extfile(LibFileVAT3)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100171024     D §ctrno          s              6  0 INZ(*zeros)
002200171024     D wFGS            s              3    inz
002300010201
002400171024
002500171024     D LibFileVAB7     s             21A   inz
002600171024     D LibFileVAT3     s             21A   inz
002700171024     D currSysNeta     s              8A   inz('*NULL')
002800091006
002900171024
003000091006     D*------------------
003100091006     D* LINKING A DEFINIZIONI ESTERNE
003200091006     D*------------------
003300091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003400091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003500171024     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003600171024     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003700171024     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
003800091006
003900010201
004000171024     C                   reset                   rrnum
004100171024     C                   reset                   §ctrno
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400171024     C*
004500171024     C* Reperisco il sistema AS/400 corrente
004600171024     C                   callp     UBRTVNETA_Rtv(currSysNeta)
004700171024     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
004800180125     C                   eval      LibFileVAB7 = 'FILTRAGRU/EDIVAB7I'
004900180125     C                   eval      LibFileVAT3 = 'FILTRAGRU/EDIVAT3L'
005000171024     C                   else
005100180125     C                   eval      LibFileVAB7 = 'FILTRAGRPF/EDIVAB7I'
005200180125     C                   eval      LibFileVAT3 = 'FILTRAGRPF/EDIVAT3L'
005300171024     C                   endif
005400171024     C*
005500171024     C* Apertura file "overraidati"
005600171024     C                   if        not %open(EDIVAB7I)
005700171024     C                   open      EDIVAB7I
005800171024     C                   endif
005900171024     C                   if        not %open(EDIVAT3L)
006000171024     C                   open      EDIVAT3L
006100171024     C                   endif
006200000613     C*
006300171024     C                   exsr      CarTab
006400171024     C                   exsr      DefCam
006500171024     C                   exsr      ExeAbilBolle
006600171024     C*
006700171024     C* Chiusura file "overraidati"
006800171024     C                   if        %open(EDIVAB7I)
006900171024     C                   close     EDIVAB7I
007000171024     C                   endif
007100171024     C                   if        %open(EDIVAT3L)
007200171024     C                   close     EDIVAT3L
007300171024     C                   endif
007400000616     C*
007500010201     C                   seton                                        LR
007600171024
007700000801
007800171024     C     ExeAbilBolle  BEGSR
007900990910     C*
008000990914     C                   if        not %open(tivin00r)
008100990908     C                   open      tivin00r
008200990914     C                   endif
008300990910     C*
008400921023     C                   DO        *HIVAL
008500990913     C*
008600990915     C                   READ      tivin00r                               70
008700050627     C                   if        vindta > *blanks
008800000613     C                   add       1             rrnum
008900000801     C*
009000000801     C                   if        *in70 = *off
009100000801     C                             and
009200000801     C                             (vinflg = *blanks
009300000801     C                              or vinflg = '0'
009400000801     C                              or vinflg = '2')
009500000801     C*
009600000801     C                   clear                   vinmsg
009700171024     C                   eval      vinflg = '1'
009800171024     C*
009900180126     C* Innanzitutto chaino la bolla direttamente su EDIVAB
010000180126     C*   Siccome il file CONFERME è l'ARTAL con sole le righe da sbloccare
010100180126     C*   e nell'ARTAL non c'è FGS
010200180126     C*   questo deve essere sempre 0
010300180126     C                   eval      VABFGS = 0
010400180126     C*   questo test assurdo lo lascio perchè non è detto che un giorno non decidano di passarlo
010500171024     C                   if        VABFGS = *zeros
010600171024     C                   eval      VABFGS = %dec(wFGS:3:0)
010700171024     C                   endif
010800171221     C                   clear                   VABRMA
010900171221     C                   eval      VABRMA = %trim(%subst(vindta:578:11))
011000171024     C     KEYvab7_C     chain     edivab7i
011100171024     C                   if        %found(edivab7i)
011200171024     C*
011300171024     C* Prima abilito i dettagli VAT
011400171024     C     KEYvat3_P     setll     edivat3l
011500171024     C                   if        %equal(edivat3l)
011600171024     C     KEYvat3_P     reade     edivat3l
011700171024     C                   dow       not %eof(edivat3l)
011800171024     C                   eval      vatFGS = vlrPOI
011900171024     C                   update    edivat00
012000171024     C     KEYvat3_P     reade     edivat3l
012100171024     C                   enddo
012200171024     C                   endif
012300171024     C*
012400171024     C* Quindi abilito le testate VAB
012500171024     C                   eval      vabFGS = vlrPOI
012600171024     C                   update    edivab00
012700171024     C*
012800171024     C                   else
012900171024     C                   eval      vinflg = '2'
013000171024     C                   add       1             §ctrno
013100171024     C                   endif
013200000905     C*
013300000905     C                   else
013400000905     C                   eval      vinflg = '1'
013500050628     C                   endif
013600171024     C                   endif
013700000905     C*
013800000905     C  N70              update    tivin000
013900000905     C*
014000991022     C  N70              ENDdo
014100010202     C*
014200990910     C* Se non ci sono record con errori ...
014300000710     C                   if        §ctrno = 0
014400990910     C* ... restituisco esito OK.
014500990921     C                   eval      wrkesito = '0'
014600990910     C                   else
014700171024     C                   if        §ctrno < rrnum
014800990921     C                   eval      wrkesito = '1'
014900000710     C                   else
015000000710     C                   eval      wrkesito = '2'
015100990910     C                   endif
015200000710     C                   endif
015300990910     C*
015400990914     C                   if        %open(tivin00r)
015500990908     C                   close     tivin00r
015600990914     C                   endif
015700990920     C*
015800910830     C                   ENDSR
015900000613     C***
016000171024
016100171024
016200000801     C*----------------------------------------------------*
016300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
016400000801     C*----------------------------------------------------*
016500171024     C     DEFCAM        BEGSR
016600000801     C*
016700020619     C* Imposto i valori di default...
016800171020     C                   Z-ADD     0898888       VABCCM
016900171020     C                   Z-ADD     089           VABLNP
017000171024     C*
017100020619     C* ... e poi verifico se sono stati passati come parametri
017200020619     C                   IF        vlrppt > *blanks
017300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017500020619     C                   EXSR      CHKNUM
017600020619     C                   IF        PiInt=*on
017700020619     C                   Z-ADD     PiVal         VABCCM
017800020619     C                   ENDIF
017900040506     C                   ENDIF
018000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABLNP
018500040506     C                   ENDIF
018600020619     C                   ENDIF
018700171024     C                   ENDIF
018800000801     C*
018900000801     C                   ENDSR
019000171024
019100171024
019200171024     C*
019300171024     C     CarTab        begsr
019400171024     C*
019500171024     C* Reperimento Codice Filiale stabilito per "bolle inattive"
019600171024     C                   eval      iS713TBECOD = 'VPR'
019700171024     C                   eval      iS713TBEKE1 = '*VAS'
019800171024     C                   eval      iS713TBEKE2 = '*FGSANN'
019900171024     C                   eval      iS713TBELIN = *blanks
020000171024     C                   eval      iS713TBESIF = *blanks
020100171024     C                   call      'TIS713R'
020200171024     C                   parm                    iS713TBECOD
020300171024     C                   parm                    iS713TBEKE1
020400171024     C                   parm                    iS713TBEKE2
020500171024     C                   parm                    iS713TBELIN
020600171024     C                   parm                    iS713TBESIF
020700171024     C                   parm                    oS713TBEUNI
020800171024     C                   parm                    oS713ESITO
020900171024     C*
021000171024     C                   if        oS713ESITO = '1'
021100171024     C                   eval      wFGS = %trim(oS713TBEUNI)
021200171024     C                   endif
021300171024     C*
021400171024     C                   endsr
021500171024     C*
021600171024
021700171024
021800171024     C     CHKNUM        BEGSR
021900171024     C*
022000171024     C                   IF        PiDecChr = *blanks
022100171024     C                   EVAL      PiDecChr = ','
022200171024     C                   ENDIF
022300171024     C*
022400171024     C                   callp(e)  UBISNUM_Check(PiStr
022500171024     C                                          :PiDecChr
022600171024     C                                          :PiVal
022700171024     C                                          :PiNum
022800171024     C                                          :PiInt)
022900171024     C*
023000171024     C                   IF        %error
023100171024     C                   EVAL      PiInt=*off
023200171024     C                   ENDIF
023300171024     C*
023400171024     C                   ENDSR
023500070411
023600990910
023700000613     C     *inzsr        BEGSR
023800990910     C*
023900990910     C     *entry        plist
024000990920     C                   parm                    tivlrds
024100990921     C                   parm      wrkesito      esito
024200000724     C                   parm                    prmlit
024300000710     C                   parm                    prmfir
024400171024     C*
024500171024     C* Definizione chiavi
024600171024     C*
024700171024     C* Chiave su EDIVAB7I - Completa
024800171024     C     KEYvab7_C     KLIST
024900171024     C                   KFLD                    vabFGS
025000171024     C                   KFLD                    vabCCM
025100171024     C                   KFLD                    vabRMA
025200171024     C*
025300171024     C* Chiave su EDIVAT3P - Parziale
025400171024     C     KEYvat3_P     KLIST
025500171024     C                   KFLD                    vabAAS
025600171024     C                   KFLD                    vabLNP
025700171024     C                   KFLD                    vabNRS
025800171024     C                   KFLD                    vabNSP
025900000830     C*
026000000613     C                   ENDSR
026100000613     C***
