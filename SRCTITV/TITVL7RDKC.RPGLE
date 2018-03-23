000100050527      /TITLE Upload via Internet: traduzione in EDIVAB/EDIVAT. (x SAFILO  Cod. 0061602)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FEDIVABwr  O    E             DISK    usropn
000700050408     FEDIVATwr  O    E             DISK    usropn
000800050527     Ftitvl7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000050527     Ftitvl7ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300050527     D depspe          s              8    INZ(*blanks)
003400050527     D curspe          s              8    INZ(*blanks)
003500050217     D parccm          s              8    INZ(*blanks)
003600050217     D parmbr          s             10    INZ(*blanks)
003700050217     D paropz          s              1    INZ(*blanks)
003800050217     D chkcall         s              1    INZ(*blanks)
003900050527     D wSkRMA          s              8    INZ(*blanks) DIM(300)
004000050527     D si_contr        s             12    INZ(*blanks)
004100010330     D*------------------
004200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300010330     D*------------------
004400010330     D WLBDA8          DS                  INZ
004500010330     D  G08DAT                 1      8  0
004600010330     D  G08INV                 9     16  0
004700010330     D  G08ERR                17     17
004800010330     D  G08TGI                18     22  0
004900050530     D*------------------
005000050530     D* DS REPERIMENTO NUMERATORE
005100050530     D*------------------
005200050530     D trul33ds      e ds                  inz
005300050530     D*------------------
005400050530     D* DS ARCHITETTURA
005500050530     D*------------------
005600050530     D kpjba         e ds                  inz
005700050530     D*------------------
005800000313     D*
005900010330
006000010330
006100990915     C                   time                    wrkdata
006200990915     C                   time                    wrkora
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010601     C*
006700010601     C                   exsr      opeini
006800010605     C                   exsr      rwvab
006900050217     C*
007000050217     C* Effettuo la chiamata al CLLE preposto
007100050412     C                   call(e)   'TITVEVTC'
007200050217     C                   parm                    parccm
007300050217     C                   parm                    parmbr
007400050217     C                   parm      '2'           paropz
007500010601     C*
007600010601     C                   seton                                        lr
007700010601
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010601     C*--------------------------------------------------------
008400010601     C     PREELA        BEGSR
008500010601     C*
008600010601     C* SE OCCORRE SPEDIRE IN FILIALE
008700010601     C                   if        invfil <> *zeros and
008800010601     C                             flgGiro = '0'
008900010601     C*
009000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010601     C                   eval      flgGiro = '1'
009200000724     C*
009300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500010601     C     invfil        chain     azorg01l
009600000724     C                   if        %found
009700000724     C                   movel(p)  CMD(1)        depcmd
009800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009900000724     C*
010000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000724     C                   PARM                    depcmd
010400000724     C                   PARM                    LENGH
010500000724     C*
010600000724     C                   endif
010700000616     C*
010800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010900000616     C                   movel(p)  CMD(2)        depcmd
011000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011100000616     C*
011200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    depcmd
011600000616     C                   PARM                    LENGH
011700000613     C*
011800050527     C                   if        not %open(titvl7ps)
011900050527     C                   open      titvl7ps
012000000616     C                   except    testdett
012100000613     C                   endif
012200010601     C*
012300010601     C                   endif
012400010601     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900010601
013000010601     C*--------------------------------------------------------
013100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013200010601     C*--------------------------------------------------------
013300010601     C     ENDELA        BEGSR
013400990915     C*
013500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013600000613     C*
013700050527     C                   if        %open(titvl7ps)
013800000616     C                   except    findett
013900050527     C                   close     titvl7ps
014000000613     C                   endif
014100000616     C*
014200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    CMD(3)
014600000616     C                   PARM                    LENGH
014700000616     C*
014800010601     C                   ENDSR
014900010601     C***
015000010601
015100010601
015200000613
015300000613     C*--------------------------------------------------------
015400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015500000613     C*--------------------------------------------------------
015600000621     C     STPR          BEGSR
015700000613     C*
015800050527     C                   if        not %open(titvl7p)
015900050527     C                   open      titvl7p
016000990915     C                   endif
016100990915     C*
016200990915     C                   except    riepilogo
016300990915     C*
016400050527     C                   if        %open(titvl7p)
016500050527     C                   close     titvl7p
016600990914     C                   endif
016700000613     C*
016800000613     C                   ENDSR
016900000613     C***
017000000613
017100010601
017200010601
017300010330     C*--------------------------------------------------------
017400050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVAB0F              *
017500010330     C*--------------------------------------------------------
017600010605     C     RWVAB         BEGSR
017700010330     C*
017800010330     C                   if        not %open(tivin00r)
017900010330     C                   open      tivin00r
018000010330     C                   endif
018100050408     C                   if        not %open(edivabwr)
018200050408     C                   open      edivabwr
018300010330     C                   endif
018400050217     C*
018500050408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
018600050217     C                   exsr      prevat
018700050217     C*
018800050217     C                   if        chkcall = '0'
018900050217     C*
019000050408     C                   if        not %open(edivatwr)
019100050408     C                   open      edivatwr
019200050217     C                   endif
019300010330     C*
019400010604     C                   clear                   §CTROK
019500010604     C                   clear                   §CTRMO
019600010604     C                   clear                   §CTRNO
019700010330     C*
019800010330     C                   DO        *HIVAL
019900010330     C*
020000010330     C                   READ      tivin00r                               70
020100010618     C*
020200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020300010618     C                   if        vinflg = '1'
020400010618     C                   eval      flgOk = '1'
020500010618     C                   endif
020600010618     C*
020700010330     C                   if        vindta > *blanks
020800010330     C                   add       1             rrnum
020900010330     C*
021000010601     C                   if        *in70 = *off and
021100010330     C                             (vinflg = *blanks
021200010330     C                              or vinflg = '0'
021300010330     C                              or vinflg = '2')
021400010330     C*
021500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021600010711     C                   if        vinflg = *blanks or vinflg = '0'
021700010711     C                   clear                   vinmsg
021800010711     C                   endif
021900050527     C*
022000050527     C* A rottura del campo raggruppamento indicato dal cliente => elaboro
022100050527     C                   EVAL      curspe=%subst(vindta:377:8)
022200050527     C                   EVAL      si_contr=%subst(vindta:280:12)
022300050527     C*
022400050527     C                   if        depspe = *blanks                             => 1° giro
022500050527     C                   eval      depspe = curspe                              => memorizz. spediz
022600050527     C                   exsr      inzvar                                       => inizial. wrk
022700050527     C                   exsr      defcam                                       => definiz. default
022800050527     C                   exsr      impvab                                       => carico VAB
022900050527     C                   exsr      wrivat                                       => carico/scrivo VAT
023000050527     C                   else
023100050527     C                   if        curspe <> depspe OR                          => rottura di spediz
023200050527     C                             si_contr <> '000000000,00' OR                => sped. corr. contr
023300050527     C                             VABCAS <> *zeros                             => sped. prec. contr
023400050527     C                   eval      depspe = curspe                              => memorizz. spediz
023500050527     C                   exsr      wrivab                                       => scrivo VAB
023600050527     C                   exsr      inzvar                                       => inizial. wrk
023700050527     C                   exsr      defcam                                       => definiz. default
023800050527     C                   exsr      impvab                                       => carico VAB
023900050527     C                   exsr      wrivat                                       => carico/scrivo VAT
024000050527     C                   else                                                   => x stessa spediz
024100050527     C                   exsr      impvab                                       => carico VAB
024200050527     C                   exsr      wrivat                                       => carico/scrivo VAT
024300050527     C                   endif
024400050527     C                   endif
024500050527     C                   endif
024600010601     C*
024700050527     C                   exsr      PREELA
024800010601     C*
024900010604     C* Ebbene...
025000010604     C*
025100010604     C  N31              ADD       1             §CTROK            7 0
025200010604     C   32              ADD       1             §CTRMO            7 0
025300010604     C   31              ADD       1             §CTRNO            7 0
025400010604     C*
025500010604     C                   if        *in31 = *off and
025600010604     C                             *in32 = *off
025700010604     C                   eval      vinflg = '1'
025800010604     C                   else
025900010604     C                   eval      recko = vindta
026000010604     C                   except    rigadett
026100010604     C                   eval      vinflg = '2'
026200010604     C                   endif
026300010604     C*
026400010330     C                   else
026500010330     C                   eval      vinflg = '1'
026600010330     C                   endif
026700010601     C*
026800010601     C  N70              update    tivin000
026900010330     C*
027000010330     C  N70              ENDdo
027100050527     C*
027200050527     C* Scarico i VAB rimasti "in sospeso"
027300050527     C                   exsr      wrivab
027400010601     C*
027500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027600010601     C                   if        cntNonEl = *zeros or
027700010601     C                             flgMulti = '0'
027800010330     C* Se non ci sono record con errori ...
027900010601     C                   if        §ctrno = 0 and
028000010604     C                             §ctrmo = 0 and
028100010601     C                             flgStato <> '2'
028200010330     C* ... restituisco esito OK.
028300010330     C                   eval      wrkesito = '0'
028400010330     C                   else
028500010330     C                   if        §ctrok > 0
028600010330     C                   eval      wrkesito = '1'
028700010330     C                   else
028800010615     C                   if        flgOk = '0'
028900010615     C                   eval      wrkesito = '2'
029000010615     C                   else
029100010615     C                   eval      wrkesito = '6'
029200010615     C                   endif
029300010330     C                   endif
029400010330     C                   endif
029500010601     C                   else
029600010601     C                   eval      wrkesito = '9'
029700010601     C                   endif
029800050217     C*
029900050217     C                   else
030000050217     C                   eval      wrkesito = '2'
030100050217     C                   endif
030200010330     C*
030300010330     C                   if        %open(tivin00r)
030400010330     C                   close     tivin00r
030500010330     C                   endif
030600050408     C                   if        %open(edivabwr)
030700050408     C                   close     edivabwr
030800010330     C                   endif
030900050408     C                   if        %open(edivatwr)
031000050408     C                   close     edivatwr
031100050217     C                   endif
031200010601     C*
031300010601     C                   if        vlrpoi <> 999
031400010601     C                   eval      invfil = vlrpoi
031500010601     C                   endif
031600010330     C*
031700010330     C                   if        §ctrok > 0
031800010601     C                             and invfil > *zeros
031900010330     C                   exsr      invio
032000010330     C                   endif
032100010601     C*
032200010618     C                   if        flgGiro = '1'
032300010601     C                   exsr      endela
032400010618     C                   endif
032500010330     C*
032600010330     C                   ENDSR
032700010330     C***
032800010330
032900010601
033000010601
033100010601
033200010601
033300010330     C*----------------------------------------------------*
033400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033500010330     C*----------------------------------------------------*
033600010330     C     INZVAR        BEGSR
033700010330     C*
033800050527     C                   CLEAR                   wSkRMA
033900050527     C                   MOVEL     *blanks       wRMA              8
034000050527     C                   Z-ADD     *zeros        wJSkRMA           4 0
034100020204     C                   Z-ADD     *zeros        Num5_0            5 0
034200050527     C                   Z-ADD     *zeros        wPesoKG          18 5
034300050527     C                   Z-ADD     *zeros        wVolumeMC        18 5
034400020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034500020322     C                   MOVEL     '0'           FlgCAS            1
034600010330     C*
034700010330     C                   ENDSR
034800010330     C*----------------------------------------------------*
034900020722     C*  IMPOSTAZIONE CAMPI COSTANTI
035000010330     C*----------------------------------------------------*
035100010330     C     DEFCAM        BEGSR
035200010330     C*
035300020204     C* Inizializzo il buffer del record da scrivere
035400050527     C                   CLEAR                   EDIVAB00
035500050408     C                   CLEAR                   EDIVAT00
035600020204     C* Imposto i valori di default...
035700050527     C                   EVAL      VABCCM = 0061602
035800050527     C                   EVAL      VABLNP = 006
035900050527     C                   EVAL      VABCTR = 300
036000041108     C                   EVAL      VABCBO = '1'
036100050217     C                   EVAL      VABCTM = '7Q'
036200020204     C* ... e poi verifico se sono stati passati come parametri
036300020204     C                   IF        vlrppt > *blanks
036400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
036500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036600020204     C                   EXSR      CHKNUM
036700020204     C                   IF        PiInt=*on
036800020204     C                   Z-ADD     PiVal         VABCCM
036900020204     C                   ENDIF
037000040301     C                   ENDIF
037100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
037200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037300020204     C                   EXSR      CHKNUM
037400020204     C                   IF        PiInt=*on
037500020204     C                   Z-ADD     PiVal         VABLNP
037600020204     C                   ENDIF
037700040301     C                   ENDIF
037800040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
037900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038000020204     C                   EXSR      CHKNUM
038100020204     C                   IF        PiInt=*on
038200020204     C                   Z-ADD     PiVal         VABCTR
038300040301     C                   ENDIF
038400020204     C                   ENDIF
038500020204     C                   ENDIF
038600020204     C*
038700010330     C                   ENDSR
038800010607     C*----------------------------------------------------*
038900050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
039000010607     C*----------------------------------------------------*
039100010607     C     IMPVAB        BEGSR
039200010607     C*
039300010607     C                   SETOFF                                       3132
039400050606     C*
039500050606     C* VALORIZZO CAMPI RELATIVI AL "CMR"
039600050606     C                   EVAL      VABCMR = 'SAFILO' + datora_A
039700050606     C                   EVAL      VABDCM = DATCOR
039800050606     C                   EVAL      VABDTS = DATCOR
039900050606     C                   EVAL      VABHMS = ORACOR
040000050606     C                   EVAL      VABCNT = 1
040100050606     C*
040200050606     C* Effettuo considerazioni x elaborazioni "multi-filiale"
040300050606     C                   eval      depfil = VABLNP
040400050606     C                   exsr      repfil
040500050606     C                   if        depfil = invfil
040600050606     C                   if        vlrpoi = 999
040700050606     C                   MOVE(P)   invfil        VABFGS
040800050606     C                   else
040900050606     C                   MOVE(P)   vlrpoi        VABFGS
041000050606     C                   endif
041100050606     C                   endif
041200050527     C*
041300050527     C* Reperimento campi ALFA
041400050527     C*
041500050527     C* Considerazioni sulla ragione sociale del destinatario da indicare
041600050527     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
041700050527     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041800050527     C     '@':'A'       XLATE     VABRSD        VABRSD
041900050527     C* ==
042000050527     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
042100050527     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
042200050527     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
042300050527     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
042400050527     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
042500050527     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
042600050527     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
042700050530     C                   EVAL      VABRMO=%trim(%subst(vindta:377:8))
042800050527     C*
042900050527     C* Memorizzo riferimenti bolle attuale se NN già memorizzati
043000050527     C***  1° Riferimento
043100050527     C                   EVAL      wRMA=%subst(vindta:187:8)
043200050527     C     wRMA          LOOKUP    wSkRMA                                 55
043300050527     C                   IF        not %found
043400050527     C                   ADD       1             wJSkRMA
043500050527     C                   EVAL      wSkRMA(wJSkRMA) = wRMA
043600050527     C                   ENDIF
043700050527     C***  2° Riferimento
043800050527     C                   EVAL      wRMA=%subst(vindta:207:8)
043900050527     C     wRMA          LOOKUP    wSkRMA                                 55
044000050527     C                   IF        not %found
044100050527     C                   ADD       1             wJSkRMA
044200050527     C                   EVAL      wSkRMA(wJSkRMA) = wRMA
044300050527     C                   ENDIF
044400050527     C***  3° Riferimento
044500050527     C                   EVAL      wRMA=%subst(vindta:217:8)
044600050527     C     wRMA          LOOKUP    wSkRMA                                 55
044700050527     C                   IF        not %found
044800050527     C                   ADD       1             wJSkRMA
044900050527     C                   EVAL      wSkRMA(wJSkRMA) = wRMA
045000050527     C                   ENDIF
045100050527     C*
045200050527     C                   IF        %subst(vindta:16:1)<>'0'
045300050527     C                   EVAL      VABFFD='S'
045400050527     C                   ENDIF
045500050527     C                   IF        %subst(vindta:390:1)<>*blanks
045600050527     C                   EVAL      VABCBO='2'
045700050527     C                   ENDIF
045800050527     C*
045900050527     C* Reperimento campi NUMERICI
046000050527     C                   MOVEL     DATCOR        VABAAS
046100050527     C                   MOVE      DATCOR        VABMGS
046200050527     C* NSP/RMN
046300050527     C                   EVAL      PiStr=%trim(%subst(vindta:377+1:8-1))
046400050527     C                   EXSR      CHKNUM
046500050527     C                   IF        PiInt=*on
046600050527     C                   Z-ADD     PiVal         VABNSP
046700050527     C                   Z-ADD     PiVal         VABRMN
046800050527     C                   ELSE
046900050530     C* NSP => Stacco un numeratore da AZNUM
047000050530     C                   clear                   TRUL33DS
047100050530     C                   eval      I33OPE = *zeros
047200050530     C                   eval      I33CNU = 302
047300050530     C                   eval      I33NUM = 1
047400050530     C                   movel     TRUL33DS      KPJBU
047500050530     C                   call      'TRUL33R'
047600050530     C                   parm                    KPJBA
047700050530     C                   movel     KPJBU         TRUL33DS
047800050530     C                   if        O33ERR = *zeros
047900050530     C                   movel     '2005'        O33NRF
048000050530     C                   z-add     O33NRF        VABNSP
048100050530     C                   z-add     O33NRF        VABRMN
048200050530     C                   else
048300050527     C                   SETON                                        31
048400050527     C                   Z-ADD     *zeros        VABNSP
048500050527     C                   Z-ADD     *zeros        VABRMN
048600050527     C                   EVAL      vinmsg = %trimr(vinmsg)
048700050527     C                             + ' ' + 'VABNSP VABRMN'
048800050527     C                   ENDIF
048900050530     C                   ENDIF
049000050527     C* CAD
049100050527     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
049200050527     C                   EXSR      CHKNUM
049300050527     C                   IF        PiInt=*on
049400050527     C                   Z-ADD     PiVal         Num5_0
049500050527     C                   MOVEL(p)  Num5_0        VABCAD
049600050527     C                   ELSE
049700050527     C                   SETON                                        32
049800050527     C                   EVAL      VABCAD = *zeros
049900050527     C                   EVAL      vinmsg = %trimr(vinmsg)
050000050527     C                             + ' ' + 'VABCAD'
050100050527     C                   ENDIF
050200050527     C* NCL
050300050527     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
050400050527     C                   EXSR      CHKNUM
050500050527     C                   IF        PiInt=*on
050600050527     C                   ADD       PiVal         VABNCL
050700050527     C                   ELSE
050800050527     C                   SETON                                        32
050900050527     C                   Z-ADD     *zeros        VABNCL
051000050527     C                   EVAL      vinmsg = %trimr(vinmsg)
051100050527     C                             + ' ' + 'VABNCL'
051200050527     C                   ENDIF
051300050527     C* PKB
051400050527     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
051500050527     C                   EXSR      CHKNUM
051600050527     C                   IF        PiNum=*on
051700050527     C                   ADD       PiVal         wPesoKG
051800050527     C                   ELSE
051900050527     C                   SETON                                        32
052000050527     C                   ADD       *zeros        wPesoKG
052100050527     C                   EVAL      vinmsg = %trimr(vinmsg)
052200050527     C                             + ' ' + 'VABPKB'
052300050527     C                   ENDIF
052400050527     C* VLB
052500050527     C                   IF        %subst(vindta:308:7)<>*blanks
052600050527     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
052700050527     C                   EXSR      CHKNUM
052800050527     C                   IF        PiNum=*on
052900050527     C                   ADD       PiVal         wVolumeMC
053000050527     C                   ELSE
053100050527     C                   SETON                                        32
053200050527     C                   ADD       *zeros        wVolumeMC
053300050527     C                   EVAL      vinmsg = %trimr(vinmsg)
053400050527     C                             + ' ' + 'VABVLB'
053500050527     C                   ENDIF
053600050527     C                   ENDIF
053700050527     C* CAS
053800050527     C                   IF        %subst(vindta:280:12) <> '000000000,00'
053900050527     C                   EVAL      FlgCAS = '1'
054000050527     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
054100050527     C                   EXSR      CHKNUM
054200050527     C                   IF        PiNum=*on
054300050527     C                   Z-ADD     PiVal         VABCAS
054400050527     C                   ELSE
054500050527     C                   SETON                                        32
054600050527     C                   Z-ADD     *zeros        VABCAS
054700050527     C                   EVAL      vinmsg = %trimr(vinmsg)
054800050527     C                             + ' ' + 'VABCAS'
054900050527     C                   ENDIF
055000050527     C                   ENDIF
055100050527     C* IAS
055200050527     C                   IF        %subst(vindta:295:12) <> '000000000,00'
055300050527     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
055400050527     C                   EXSR      CHKNUM
055500050527     C                   IF        PiNum=*on
055600050527     C                   Z-ADD     PiVal         VABIAS
055700050527     C                   ELSE
055800050527     C                   SETON                                        32
055900050527     C                   Z-ADD     *zeros        VABIAS
056000050527     C                   EVAL      vinmsg = %trimr(vinmsg)
056100050527     C                             + ' ' + 'VABIAS'
056200050527     C                   ENDIF
056300050527     C                   ENDIF
056400020322     C*
056500020322     C* Considerazioni finali su CBO/CAS
056600020322     C                   IF        FlgCAS = '1'
056700040713     C                   IF        VABCBO = '1'
056800020322     C                   EVAL      VABCBO = '4'
056900040713     C                   ENDIF
057000040713     C                   IF        VABCBO = '2'
057100040713     C                   EVAL      VABCBO = '6'
057200040713     C                   ENDIF
057300020322     C                   ENDIF
057400020204     C*
057500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057600020204     C                   EXSR      CHKIMPDIV
057700020204     C*
057800010607     C                   ENDSR
057900010607     C*----------------------------------------------------*
058000010601
058100020204
058200020204     C*----------------------------------------------------*
058300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058400020204     C*----------------------------------------------------*
058500020204     C     CHKIMPDIV     BEGSR
058600020204     C*
058700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058800020204     C                   Z-ADD     *zeros        wrkDec            9 9
058900020204     C*
059000020204     C* Come prima cosa effettuo considerazioni sulla divisa
059100020204     C                   IF        vabIAS > *zeros
059200020204     C                   IF        vabVAS <> 'EUR'
059300020204     C                   EVAL      vabVAS =  'ITL'
059400020204     C                   ENDIF
059500020204     C                   ENDIF
059600020204     C*
059700020204     C                   IF        vabCAS > *zeros
059800020204     C                   IF        vabVCA <> 'EUR'
059900020204     C                   EVAL      vabVCA =  'ITL'
060000020204     C                   ENDIF
060100020204     C                   ENDIF
060200020204     C*
060300020204     C                   IF        vabVMD > *zeros
060400020321     C                   IF        vabVAD <> 'EUR'
060500020204     C                   EVAL      vabVAD =  'ITL'
060600020204     C                   ENDIF
060700020204     C                   ENDIF
060800020204     C*
060900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061000020204     C                   Z-ADD     vabIAS        wrkDec
061100020204     C                   IF        wrkDec > *zeros
061200020204     C                   IF        vabVAS = 'ITL'
061300020204     C                   EVAL      vabIAS = *zeros
061400020204     C                   ENDIF
061500020204     C                   ENDIF
061600020204     C*
061700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
061800020204     C                   Z-ADD     vabCAS        wrkDec
061900020204     C                   IF        wrkDec > *zeros
062000020204     C                   IF        vabVCA = 'ITL'
062100020204     C                   EVAL      vabCAS = *zeros
062200020204     C                   ENDIF
062300020204     C                   ENDIF
062400020204     C*
062500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062600020204     C                   Z-ADD     vabVMD        wrkDec
062700020204     C                   IF        wrkDec > *zeros
062800020204     C                   IF        vabVAD = 'ITL'
062900020204     C                   EVAL      vabVMD = *zeros
063000020204     C                   ENDIF
063100020204     C                   ENDIF
063200020204     C*
063300020204     C                   ENDSR
063400020204     C***
063500020204
063600010330
063700010330
063800010330     C*----------------------------------------------------*
063900010330     C*  CONTROLLO NUMERICITA' CAMPI
064000010330     C*----------------------------------------------------*
064100010330     C     CHKNUM        BEGSR
064200010330     C*
064300010606     C                   IF        PiDecChr = *blanks
064400050217     C                   EVAL      PiDecChr = ','
064500010606     C                   ENDIF
064600010606     C*
064700010606     C                   CALL(e)   'ISNUMERIC'
064800010330     C                   PARM                    PiStr            30
064900010606     C                   PARM                    PiDecChr          1
065000010330     C                   PARM      *ZEROS        PiVal            30 9
065100010330     C                   PARM      '0'           PiInt             1
065200010330     C                   PARM      '0'           PiNum             1
065300010330     C                   IF        %error
065400010606     C                   EVAL      PiNum=*off
065500010330     C                   ENDIF
065600010330     C*
065700010330     C                   ENDSR
065800010330     C***
065900050217
066000050217
066100050217
066200050217     C*----------------------------------------------------*
066300050408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
066400050217     C*----------------------------------------------------*
066500050217     C     PREVAT        BEGSR
066600050217     C*
066700050408     C* Compongo il nome del membro da dare al EDIVATWR
066800050217     C                   eval      parmbr = vlrhdl
066900050217     C                   movel     'M'           parmbr
067000050217     C                   eval      parccm = %subst(vlrKSC:2:7)
067100050217     C                   eval      paropz = '1'
067200050217     C* Effettuo la chiamata al CLLE preposto
067300050412     C                   call(e)   'TITVEVTC'
067400050217     C                   parm                    parccm
067500050217     C                   parm                    parmbr
067600050217     C                   parm                    paropz
067700050217     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067800050217     C                   if        %error
067900050217     C                   movel     '1'           chkcall
068000050217     C                   else
068100050217     C                   movel     '0'           chkcall
068200050217     C                   endif
068300050217     C*
068400050217     C                   ENDSR
068500050527
068600050527
068700050527
068800050527     C*----------------------------------------------------*
068900050527     C*  SCARICO BUFFER FILE EDIVAB
069000050527     C*----------------------------------------------------*
069100050527     C     WRIVAB        BEGSR
069200050527     C*
069300050527     C* VALORIZZAZIONI/EQUIVALENZE VALORI DA SOMMATORIE
069400050527     C                   EVAL(H)   VABPKB = wPesoKG
069500050527     C                   EVAL(H)   VABVLB = wVolumeMC / 1000000
069600050527     C*
069700050527     C* SPALMO I VARI RIFERIMENTI MITTENTE MEMORIZZATI SULLE VARIE BOLLE ACCORPATE NEI CAMPI
069800050527     C* CHE ABBIAMO A DISPOSIZIONE
069900050527     C                   Z-ADD     1             wJSkRMA
070000050527     C                   DOW       wJSkRMA <= %elem(wSkRMA)
070100050527     C                   IF        wSkRMA(wJSkRMA) <> *blanks
070200050527     C* 1° Riferimenti mittente sul VABRMA
070300050527     C                   IF        wJSkRMA = 1
070400050527     C                   EVAL      VABRMA = wSkRMA(wJSkRMA)
070500050527     C                   ENDIF
070600050527     C* 2°/3°/4°/5° Riferimenti mittente sul VABNOT
070700050527     C                   IF        wJSkRMA = 2
070800050527     C                   EVAL      %subst(VABNOT:1:8) = wSkRMA(wJSkRMA)
070900050527     C                   ENDIF
071000050527     C                   IF        wJSkRMA = 3
071100050527     C                   EVAL      %subst(VABNOT:10:8) = wSkRMA(wJSkRMA)
071200050527     C                   ENDIF
071300050527     C                   IF        wJSkRMA = 4
071400050527     C                   EVAL      %subst(VABNOT:19:8) = wSkRMA(wJSkRMA)
071500050527     C                   ENDIF
071600050527     C                   IF        wJSkRMA = 5
071700050527     C                   EVAL      %subst(VABNOT:28:8) = wSkRMA(wJSkRMA)
071800050527     C                   ENDIF
071900050527     C* 6°/7°/8°/9° Riferimenti mittente sul VABNOT
072000050527     C                   IF        wJSkRMA = 6
072100050527     C                   EVAL      %subst(VABNT2:1:8) = wSkRMA(wJSkRMA)
072200050527     C                   ENDIF
072300050527     C                   IF        wJSkRMA = 7
072400050527     C                   EVAL      %subst(VABNT2:10:8) = wSkRMA(wJSkRMA)
072500050527     C                   ENDIF
072600050527     C                   IF        wJSkRMA = 8
072700050527     C                   EVAL      %subst(VABNT2:19:8) = wSkRMA(wJSkRMA)
072800050527     C                   ENDIF
072900050527     C                   IF        wJSkRMA = 9
073000050527     C                   EVAL      %subst(VABNT2:28:8) = wSkRMA(wJSkRMA)
073100050527     C                   ENDIF
073200050527     C*
073300050527     C* Dopo i 9 riferimenti esco xchè inutile perseverare nel ciclo x nulla
073400050527     C                   IF        wJSkRMA > 9
073500050527     C                   LEAVE
073600050527     C                   ENDIF
073700050527     C*
073800050527     C                   ENDIF
073900050527     C                   ADD       1             wJSkRMA
074000050527     C                   ENDDO
074100050527     C*
074200050527     C  N31              WRITE     EDIVAB00
074300050527     C*
074400050527     C                   ENDSR
074500050527     C*----------------------------------------------------*
074600050217
074700050217
074800050217
074900050217     C*----------------------------------------------------*
075000050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
075100050217     C*----------------------------------------------------*
075200050217     C     WRIVAT        BEGSR
075300050527     C*
075400050527     C* VALORIZZO CAMPI RELATIVI AL "CMR"
075500050527     C                   EVAL      VATCMR = VABCMR
075600050527     C                   EVAL      VATCNT = VABCNT
075700050217     C*
075800050217     C* Valorizzo il buffer del file dettagli barcode colli
075900050217     C                   EVAL      VATFGS = VABFGS
076000050217     C                   EVAL      VATAAS = VABAAS
076100050217     C                   EVAL      VATLNP = VABLNP
076200050217     C                   EVAL      VATNSP = VABNSP
076300050217     C                   EVAL      VATCCM = VABCCM
076400050527     C                   EVAL      VATTRC='E'
076500050527     C                   EVAL      VATNOT = %trim(%subst(vindta:331:14))
076600050527     C  N31              WRITE     EDIVAT00
076700050217     C*
076800050217     C                   ENDSR
076900050527     C*----------------------------------------------------*
077000010330
077100010601
077200010601
077300010601
077400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
077500010601     C     repfil        BEGSR
077600010601     C*
077700010601     C                   if        invfil = *zeros and
077800010601     C                             depfil > *zeros and
077900010629     C                             (vinflg = *blanks or
078000010629     C                              vinflg = *zeros)
078100010601     C
078200010601     C                   eval      invfil = depfil
078300010601     C                   endif
078400010601     C*
078500010601     C                   if        depfil <> invfil and
078600010601     C                             invfil > *zeros
078700010601     C                   eval      flgMulti = '1'
078800010601     C                   if        vinflg = *blanks
078900010601     C                   add       1             cntNonEl
079000010601     C                   endif
079100010601     C                   endif
079200010601     C*
079300010601     C                   if        vinflg = '2'
079400010601     C                   eval      flgStato = '2'
079500010601     C                   endif
079600010601     C*
079700010601     C                   ENDSR
079800010601     C***
079900050217
080000050217
080100050217
080200050217
080300050217      /TITLE Invio dei dati al punto operativo.
080400050217     C     invio         BEGSR
080500050217     C*
080600050408     C* 1° invio EDIVAT
080700050217     C                   reset                   dscmz
080800050217     C                   move      vlrpoi        cmzdst
080900050408     C                   eval      cmzfld = 'EDIVATWR'
081000050412     C                   eval      cmzmbd = parmbr
081100050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
081200050217     C***                if        prmfir = *blanks
081300050408     C                   eval      cmzfla = 'EDIVAT0F'
081400050408     C                   eval      cmzmba = 'EDIVAT0F'
081500050217     C***                else
081600050217     C***                eval      cmzfla = prmfir
081700050217     C***                eval      cmzmba = prmfir
081800050217     C***                endif
081900050217     C                   eval      cmznrr = *zeros
082000050217     C                   move      §ctrok        cmznrr
082100050217     C                   eval      cmzlba = vlrfl1
082200050217     C                   call(e)   'TIS711C'
082300050217     C                   parm                    dscmz
082400050217     C                   parm      *blanks       esito
082500050217     C                   if        %error
082600050217     C                             or cmzerr = '1'
082700050217     C                             or esito  = '1'
082800050217     C                   eval      wrkesito = '3'
082900050217     C                   else
083000050217     C*
083100050408     C* 2° invio EDIVAB
083200050217     C                   reset                   dscmz
083300050217     C                   move      vlrpoi        cmzdst
083400050217     C                   eval      cmzfld = vlrfou
083500050217     C                   eval      cmzmbd = vlrhdl
083600050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
083700050217     C***                if        prmfir = *blanks
083800050408     C                   eval      cmzfla = 'EDIVAB0F'
083900050408     C                   eval      cmzmba = 'EDIVAB0F'
084000050217     C***                else
084100050217     C***                eval      cmzfla = prmfir
084200050217     C***                eval      cmzmba = prmfir
084300050217     C***                endif
084400050217     C                   eval      cmznrr = *zeros
084500050217     C                   move      §ctrok        cmznrr
084600050217     C                   eval      cmzlba = vlrfl1
084700050217     C                   call(e)   'TIS711C'
084800050217     C                   parm                    dscmz
084900050217     C                   parm      *blanks       esito
085000050217     C                   if        %error
085100050217     C                             or cmzerr = '1'
085200050217     C                             or esito  = '1'
085300050217     C                   eval      wrkesito = '3'
085400050217     C                   endif
085500050217     C                   endif
085600050217     C*
085700050217     C                   ENDSR
085800050217     C***
085900050217
086000050217
086100050217
086200010601
086300010601
086400010601      /TITLE Invio dei dati al punto operativo.
086500010601     C     opeini        BEGSR
086600010601     C*
086700010601     C* Inizializzo flag e contatori operativi
086800010601     C                   movel     '0'           flgGiro           1
086900010601     C                   movel     '0'           flgMulti          1
087000010601     C                   movel     '1'           flgStato          1
087100010615     C                   movel     '0'           flgOk             1
087200010601     C                   z-add     *zeros        cntNonEl         10 0
087300010601     C                   z-add     *zeros        depfil            3 0
087400010601     C                   z-add     *zeros        invfil            3 0
087500010601     C*
087600010601     C                   ENDSR
087700010601     C***
087800010601
087900010601
088000010601
088100010330
088200010330
088300000613     C     *inzsr        BEGSR
088400990910     C*
088500990910     C     *entry        plist
088600990920     C                   parm                    tivlrds
088700990921     C                   parm      wrkesito      esito
088800000724     C                   parm                    prmlit
088900000710     C                   parm                    prmfir
089000010330     C*
089100010330     C* CALCOLA LA DATA CORRENTE
089200010330     C                   time                    wn14             14 0
089300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
089400050408     C                   movel     wn14          oracor            6 0          *ORA
089500010330     C                   z-add     wn8           g08dat
089600010330     C                   z-add     *zeros        g08inv
089700010330     C                   movel     '0'           g08err
089800010330     C                   call      'XSRDA8'
089900010330     C                   parm                    wlbda8
090000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
090100050527     C*
090200050527     C* Compongo data/ora in formato alfa
090300050527     C                   movel(P)  datcor        datora_A         14
090400050527     C                   move      oracor        datora_A
090500000613     C*
090600000613     C                   ENDSR
090700000613     C***
090800990908
090900050527     Otitvl7p   E            riepilogo         2
091000990915     O                                              'Upload via Internet'
091100990915     O                                           +1 'Traduzione TIVIN00R -'
091200050527     O                                           +1 'EDIVABWR/EDIVATWR'
091300990915     O                                           +1 'Testate bolle'
091400990915     O          E            riepilogo   2
091500990915     O                       wrkdata
091600990915     O                       wrkora              +1
091700990915     O                       procname            +1
091800990915     O          E            riepilogo   2
091900990915     O                                              'Cliente..................:'
092000010605     O                       VABCCM        z     +1
092100990915     O          E            riepilogo   2
092200990920     O                                              'Riferimento Strategi.....:'
092300990920     O                       vlrhdl              +1
092400990915     O          E            riepilogo   2
092500990915     O                                              'Giusti...................:'
092600971022     O                       §CTROK        2   +  1
092700990915     O          E            riepilogo   2
092800990915     O                                              'Sbagliati e corretti.....:'
092900971022     O                       §CTRMO        2   +  1
093000990915     O          E            riepilogo   2
093100990915     O                                              'Sbagliati e scartati.....:'
093200971022     O                       §CTRNO        2   +  1
093300000613
093400050527     Otitvl7ps  E            testdett          2
093500000613     O                                              'Upload via Internet'
093600000613     O                                           +1 'Traduzione TIVIN00R -'
093700050527     O                                           +1 'EDIVABWR/EDIVATWR'
093800000613     O                                           +1 'Testate bolle'
093900000616     O          E            testdett    3
094000000613     O                                           +2 'N° rec'
094100000613     O                                           +3 'Anteprima contenuto'
094200000616     O          E            rigadett    2
094300000613     O                       rrnum               +2
094400000621     O                       recko               +3
094500000616     O          E            findett     2
094600000613     O                       wrkdata
094700000613     O                       wrkora              +1
094800000613     O                       procname            +1
094900000616     O          E            findett     2
095000000613     O                                              'Cliente..................:'
095100010605     O                       VABCCM        z     +1
095200000616     O          E            findett     2
095300000613     O                                              'Riferimento Strategi.....:'
095400000613     O                       vlrhdl              +1
095500000616     O          E            findett     2
095600000613     O                                              'Giusti...................:'
095700000613     O                       §CTROK        2   +  1
095800000616     O          E            findett     2
095900000613     O                                              'Sbagliati e corretti.....:'
096000000613     O                       §CTRMO        2   +  1
096100000616     O          E            findett     2
096200000613     O                                              'Sbagliati e scartati.....:'
096300000613     O                       §CTRNO        2   +  1
096400000613** CMD - COMANDI CL
096500050527OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
096600050527OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
096700050527DLTOVR FILE(TITVL7P TITVL7PS) LVL(*)
096800010606
096900000613
