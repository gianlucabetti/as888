000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800091228     Ftitv1u1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100091228     Ftitv1u1ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070719     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000091228     D wScaricaBuffer  s              1    INZ(*zeros)
004100091228     D wTC             s              2    INZ
004200091228     D wText           s            160    INZ
004300091228     D wNote           s             70    INZ
004400091228     D wDCR            s              8    INZ
004500091228     D wHCR            s              4    INZ
004600041025     D*------------------
004700041025     D* DS REPERIMENTO NUMERATORE
004800041025     D*------------------
004900041025     D trul33ds      e ds                  inz
005000041025     D*------------------
005100041025     D* DS ARCHITETTURA
005200041025     D*------------------
005300041025     D kpjba         e ds                  inz
005400990908
005500010201
005600010201
005700990915     C                   time                    wrkdata
005800990915     C                   time                    wrkora
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000724     C*
006300000724     C* SE OCCORRE SPEDIRE IN FILIALE
006400000724     C                   if        vlrpoi <> *zeros
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800000724     C     vlrpoi        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011113     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200091228     C                   if        not %open(titv1u1ps)
009300091228     C                   open      titv1u1ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900070719     C*
010000070719     C* Esegue lancio TISI95R solo x chiusura
010100070719     C                   CLEAR                   TISI95DS
010200070719     C                   EVAL      I95TLA = 'C'
010300070719     C                   CALL      'TISI95R'
010400070719     C                   PARM                    TISI95DS
010500000613     C*
010600010202     C* Effettuo la chiamata al CLLE preposto
010700040506     C                   call(e)   'TITVVTC'
010800010202     C                   parm                    parccm
010900010202     C                   parm                    parmbr
011000010202     C                   parm      '2'           paropz
011100010202     C*
011200091228     C                   if        %open(titv1u1ps)
011300000616     C                   except    findett
011400091228     C                   close     titv1u1ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300010201     C                   seton                                        LR
012400000613
012500000613
012600000613     C*--------------------------------------------------------
012700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012800000613     C*--------------------------------------------------------
012900000621     C     STPR          BEGSR
013000000613     C*
013100091228     C                   if        not %open(titv1u1p)
013200091228     C                   open      titv1u1p
013300990915     C                   endif
013400990915     C*
013500990915     C                   except    riepilogo
013600990915     C*
013700091228     C                   if        %open(titv1u1p)
013800091228     C                   close     titv1u1p
013900990914     C                   endif
014000990910     C*
014100000613     C                   ENDSR
014200000613     C***
014300990908
014400000801
014500910830     C*--------------------------------------------------------
014600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014700910830     C*--------------------------------------------------------
014800040526     C     RWFILE        BEGSR
014900990910     C*
015000990914     C                   if        not %open(tivin00r)
015100990908     C                   open      tivin00r
015200990914     C                   endif
015300021113     C                   if        not %open(fivabwwr)
015400021113     C                   open      fivabwwr
015500990914     C                   endif
015600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015700020305     C                   exsr      prevat
015800010201     C*
015900010202     C                   if        chkcall = '0'
016000010202     C*
016100021113     C                   if        not %open(fivatwwr)
016200021113     C                   open      fivatwwr
016300010201     C                   endif
016400990910     C*
016500010201     C                   clear                   §CTROKVB          5 0
016600020305     C                   clear                   §CTROKVT          5 0
016700000801     C                   clear                   §CTRMO            5 0
016800000801     C                   clear                   §CTRNO            5 0
016900040910     C*
017000921023     C                   DO        *HIVAL
017100990913     C*
017200990915     C                   READ      tivin00r                               70
017300040910     C                   if        vindta > *blanks
017400000613     C                   add       1             rrnum
017500000801     C*
017600000801     C                   if        *in70 = *off
017700000801     C                             and
017800000801     C                             (vinflg = *blanks
017900000801     C                              or vinflg = '0'
018000000801     C                              or vinflg = '2')
018100000801     C*
018200000801     C                   clear                   vinmsg
018300000801     C                   eval      vinflg = '1'
018400040910     C*
018500040910     C* Eseguo routine d traduzione
018600040910     C                   exsr      impvabvat
018700040802     C*
018800010305     C                   endif
018900000905     C*
019000000905     C                   else
019100000905     C                   eval      vinflg = '1'
019200000905     C                   endif
019300000905     C*
019400000905     C  N70              update    tivin000
019500000905     C*
019600991022     C  N70              ENDdo
019700080310     C*
019800091228     C* Scarico l'ultima testata rimasta in canna
019900091228     C                   EXSR      WRIVAB
020000010202     C*
020100010202     C                   endif
020200990910
020300990910     C* Se non ci sono record con errori ...
020400000710     C                   if        §ctrno = 0
020500990910     C* ... restituisco esito OK.
020600990921     C                   eval      wrkesito = '0'
020700990910     C                   else
020800010201     C                   if        §ctrokvb > 0
020900990921     C                   eval      wrkesito = '1'
021000000710     C                   else
021100000710     C                   eval      wrkesito = '2'
021200990910     C                   endif
021300000710     C                   endif
021400990910     C*
021500990914     C                   if        %open(tivin00r)
021600990908     C                   close     tivin00r
021700990914     C                   endif
021800021113     C                   if        %open(fivabwwr)
021900021113     C                   close     fivabwwr
022000990914     C                   endif
022100021113     C                   if        %open(fivatwwr)
022200021113     C                   close     fivatwwr
022300010201     C                   endif
022400990910     C*
022500010201     C                   if        §ctrokvb > 0
022600000724     C                             and vlrpoi <> *zeros
022700010202     C                   exsr      invio
022800990920     C                   endif
022900990920     C*
023000910830     C                   ENDSR
023100000613     C***
023200990920
023300000801     C*----------------------------------------------------*
023400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023500000801     C*----------------------------------------------------*
023600010201     C     INZVAR        BEGSR
023700000801     C*
023800040802     C                   Z-ADD     *zeros        Num5_0            5 0
023900040802     C                   MOVEL     '0'           FlgCAS            1
024000091228     C                   CLEAR                   wTC
024100091228     C                   CLEAR                   wText
024200091228     C                   CLEAR                   wNote
024300091228     C                   CLEAR                   wDCR
024400091228     C                   CLEAR                   wHCR
024500000801     C*
024600000801     C                   ENDSR
024700000801     C*----------------------------------------------------*
024800040910     C*  IMPOSTAZIONE CAMPI COSTANTI
024900000801     C*----------------------------------------------------*
025000000801     C     DEFCAM        BEGSR
025100000801     C*
025200021113     C                   CLEAR                   FIVAB000
025300040802     C                   CLEAR                   FIVAT000
025400020619     C* Imposto i valori di default...
025500100113     C                   Z-ADD     2551719       VABCCM
025600100113     C                   Z-ADD     2551719       VATCCM
025700100113     C                   Z-ADD     114           VABLNP
025800100113     C                   Z-ADD     144           VATLNP
025900100113     C                   Z-ADD     001           VABCTR
026000100113     C                   MOVEL     '7Q'          VABCTM
026100040823     C                   MOVEL     '1'           VABCBO
026200020619     C* ... e poi verifico se sono stati passati come parametri
026300020619     C                   IF        vlrppt > *blanks
026400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026600020619     C                   EXSR      CHKNUM
026700020619     C                   IF        PiInt=*on
026800020619     C                   Z-ADD     PiVal         VABCCM
026900020619     C                   Z-ADD     PiVal         VATCCM
027000020619     C                   ENDIF
027100040506     C                   ENDIF
027200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027400020619     C                   EXSR      CHKNUM
027500020619     C                   IF        PiInt=*on
027600020619     C                   Z-ADD     PiVal         VABLNP
027700020619     C                   Z-ADD     PiVal         VATLNP
027800040506     C                   ENDIF
027900020619     C                   ENDIF
028000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028200020619     C                   EXSR      CHKNUM
028300020619     C                   IF        PiInt=*on
028400020619     C                   Z-ADD     PiVal         VABCTR
028500040506     C                   ENDIF
028600020619     C                   ENDIF
028700060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
028800060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
028900060202     C                   ENDIF
029000020619     C                   ENDIF
029100000801     C*
029200000801     C                   ENDSR
029300000801     C*----------------------------------------------------*
029400040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
029500000801     C*----------------------------------------------------*
029600040910     C     IMPVABVAT     BEGSR
029700040910     C*
029800040910     C* Traduzione relativa ai tipi record del file del cliente
029900040910     C*
030000071210     C***
030100091228     C* ...tipo record 'B' (nuova spedizione)
030200091228     C                   IF        %subst(vindta:1:1) = 'B'
030300080310     C*
030400080310     C* Ad ogni primo record d dettaglio scarico il buffer della testata
030500080310     C                   IF        wScaricaBuffer = '0'
030600080310     C                   EVAL      wScaricaBuffer = '1'
030700080310     C                   ELSE
030800091228     C                   EXSR      WRIVAB
030900080310     C                   ENDIF
031000080310     C*
031100071210     C* Resetto indicatore d anomalia sul singolo record
031200091228     C                   eval      vinflg = '1'
031300071210     C* ......inizializzazioni iniziali e formati record file Bartolini
031400071210     C                   EXSR      INZVAR
031500071210     C                   EXSR      DEFCAM
031600071210     C*
031700071210     C                   Z-ADD     *zeros        errore            1 0
031800071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
031900071210     C                   MOVEL     datcor        VABAAS
032000071210     C                   MOVEL     datcor        VATAAS
032100071210     C                   MOVE      datcor        VABMGS
032200071210     C                   MOVE(P)   vlrpoi        VABFGS
032300071210     C                   MOVE(P)   vlrpoi        VATFGS
032400071210     C* ......VABNSP/VATNSP
032500071210     C* NSP => Stacco un numeratore da AZNUM
032600071210     C                   clear                   TRUL33DS
032700071210     C                   eval      I33OPE = *zeros
032800071210     C                   eval      I33CNU = 302
032900071210     C                   eval      I33NUM = 1
033000071210     C                   movel     TRUL33DS      KPJBU
033100071210     C                   call      'TRUL33R'
033200071210     C                   parm                    KPJBA
033300071210     C                   movel     KPJBU         TRUL33DS
033400071210     C                   if        O33ERR = *zeros
033500071210     C                   z-add     O33NRF        VABNSP
033600071210     C                   z-add     O33NRF        VATNSP
033700071210     C                   else
033800071210     C                   Z-ADD     1             errore
033900071210     C                   EVAL      vinmsg = %trimr(vinmsg)
034000071210     C                             + ' ' + 'VABNSP VATNSP'
034100071210     C                   endif
034200091228     C*
034300091228     C                   ENDIF
034400091228     C*
034500091228     C***
034600091228     C* ...tipo record 'C' (importo da assicurare)
034700091229     C**                 IF        %subst(vindta:1:1) = 'C'
034800091228     C*
034900091228     C* Resetto indicatore d anomalia sul singolo record
035000091229     C**                 eval      vinflg = '1'
035100091228     C* ......VABIAS
035200091229     C**                 IF        %subst(vindta:104:9) <> *blanks AND
035300091229     C**                           %subst(vindta:104:9) <> *zeros
035400091229     C**                 EVAL      PiStr=%trim(%subst(vindta:104:9))
035500091229     C**                 EXSR      CHKNUM
035600091229     C**                 IF        PiNum=*on
035700091229     C**                 EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
035800091229     C**                 Z-ADD(H)  PiVal         VABIAS
035900091229     C**                 ELSE
036000091229     C**                 ADD       1             errore
036100091229     C**                 EVAL      vinmsg = %trimr(vinmsg)
036200091229     C**                           + ' ' + 'VABIAS'
036300091229     C**                 ENDIF
036400091229     C**                 ENDIF
036500091228     C* ......VABVAS
036600091229     C**                 EVAL      VABVAS=%trim(%subst(vindta:113:3))
036700091228     C*
036800091229     C**                 ENDIF
036900091228     C*
037000091228     C***
037100091228     C* ...tipo record 'D' (destinatario - ragione sociale)
037200091228     C                   IF        %subst(vindta:1:1) = 'D'
037300091228     C*
037400091228     C* Resetto indicatore d anomalia sul singolo record
037500091228     C                   eval      vinflg = '1'
037600091228     C* ......VABRSD
037700091228     C                   EVAL      VABRSD=%trim(%subst(vindta:5:35))
037800091228     C* ......VABRD2
037900091228     C                   EVAL      VABRD2=%trim(%subst(vindta:40:35))
038000091228     C*
038100091228     C                   ENDIF
038200091228     C*
038300091228     C***
038400091228     C* ...tipo record 'E' (destinatario - indirizzo)
038500091228     C                   IF        %subst(vindta:1:1) = 'E'
038600091228     C*
038700091228     C* Resetto indicatore d anomalia sul singolo record
038800091228     C                   eval      vinflg = '1'
038900091228     C* ......VABIND
039000091228     C                   EVAL      VABIND=%trim(%subst(vindta:5:35))
039100091228     C* ......VABNZD
039200091228     C                   EVAL      VABNZD=%trim(%subst(vindta:40:3))
039300091228     C                   IF        VABNZD='I'   OR
039400091228     C                             VABNZD='IT'  OR
039500091228     C                             VABNZD='ITA'
039600091228     C                   EVAL      VABNZD=*blanks
039700091228     C                   ENDIF
039800091228     C* ......VABCAD
039900091228     C                   EVAL      VABCAD=%trim(%subst(vindta:43:9))
040000091228     C* ......VABLOD
040100091228     C                   EVAL      VABLOD=%trim(%subst(vindta:52:35))
040200091228     C* ......VABPRD
040300091228     C* Reperisco la provincia dal CAP e dalla località
040400091228     C                   IF        VABPRD  = *blanks AND
040500091228     C                             VABCAD <> *blanks
040600091228     C                   CLEAR                   TISI95DS
040700091228     C                   EVAL      I95TCN = '3'
040800091228     C                   Z-ADD     datcor        I95DAT
040900091228     C                   EVAL      I95NAR = VABNZD
041000091228     C                   EVAL      I95CAP = VABCAD
041100091228     C                   EVAL      I95LOC = VABLOD
041200091228     C                   CALL      'TISI95R'
041300091228     C                   PARM                    TISI95DS
041400091228     C                   EVAL      VABPRD = O95PRV
041500091228     C                   ENDIF
041600091229     C* ......VABRMN
041700091229     C                   EVAL      PiStr=%trim(%subst(vindta:100:17))
041800091229     C                   EXSR      CHKNUM
041900091229     C                   IF        PiInt=*on
042000091229     C                   Z-ADD     PiVal         VABRMN
042100091229     C                   ELSE
042200091229     C                   Z-ADD     VABNSP        VABRMN
042300091229     C                   EVAL      vinmsg = %trimr(vinmsg)
042400091229     C                             + ' ' + 'VABRMN'
042500091229     C                   ENDIF
042600091228     C*
042700091228     C                   ENDIF
042800091228     C*
042900091228     C***
043000091228     C* ...tipo record 'F' (colli / natura merce)
043100091228     C                   IF        %subst(vindta:1:1) = 'F'
043200091228     C*
043300091228     C* Resetto indicatore d anomalia sul singolo record
043400091228     C                   eval      vinflg = '1'
043500091228     C* ......VABNCL
043600091228     C                   EVAL      PiStr=%trim(%subst(vindta:5:4))
043700091228     C                   EXSR      CHKNUM
043800091228     C                   IF        PiInt=*on
043900091228     C                   ADD       PiVal         VABNCL
044000091228     C                   ELSE
044100091228     C                   ADD       1             errore
044200091228     C                   EVAL      vinmsg = %trimr(vinmsg)
044300091228     C                             + ' ' + 'VABNCL'
044400091228     C                   ENDIF
044500091228     C* ......VABNAS
044600091228     C                   IF        %subst(vindta:37:20) <> *blanks
044700091228     C                   EVAL      VABNAS=%trim(%subst(vindta:37:20))
044800091228     C                   ENDIF
044900091228     C*
045000091228     C                   ENDIF
045100091228     C*
045200091228     C***
045300091228     C* ...tipo record 'H' ("chi sono")
045400091228     C                   IF        %subst(vindta:1:1) = 'H'
045500091228     C*
045600091228     C* Resetto indicatore d anomalia sul singolo record
045700091228     C                   eval      vinflg = '1'
045800091228     C* 1° collo
045900091228     C                   IF        %subst(vindta:8:35) <> *blanks
046000091228     C                   EVAL      VATTRC = 'E'
046100091228     C                   EVAL      VATNOT = %trim(%subst(vindta:8:35))
046200091228     C                   WRITE     FIVAT000
046300091228     C                   ADD       1             §CTROKVT
046400091228     C                   ENDIF
046500091228     C* 2° collo
046600091228     C                   IF        %subst(vindta:43:35) <> *blanks
046700091228     C                   EVAL      VATTRC = 'E'
046800091228     C                   EVAL      VATNOT = %trim(%subst(vindta:43:35))
046900091228     C                   WRITE     FIVAT000
047000091228     C                   ADD       1             §CTROKVT
047100091228     C                   ENDIF
047200091228     C* 3° collo
047300091228     C                   IF        %subst(vindta:78:35) <> *blanks
047400091228     C                   EVAL      VATTRC = 'E'
047500091228     C                   EVAL      VATNOT = %trim(%subst(vindta:78:35))
047600091228     C                   WRITE     FIVAT000
047700091228     C                   ADD       1             §CTROKVT
047800091228     C                   ENDIF
047900091228     C*
048000091228     C                   ENDIF
048100091228     C*
048200091228     C***
048300091228     C* ...tipo record 'I' (peso / volume / consegne particolari)
048400091228     C                   IF        %subst(vindta:1:1) = 'I'
048500091228     C*
048600091228     C* Resetto indicatore d anomalia sul singolo record
048700091228     C                   eval      vinflg = '1'
048800091229     C* ......VABRMA
048900091229     C                   EVAL      VABRMA=%trim(%subst(vindta:5:16))
049000091228     C* ......VABPKB
049100091228     C                   EVAL      PiStr=%trim(%subst(vindta:21:5))
049200091228     C                   EXSR      CHKNUM
049300091228     C                   IF        PiNum=*on
049400091228     C                   Z-ADD     PiVal         VABPKB
049500091228     C                   ELSE
049600091228     C                   ADD       1             errore
049700091228     C                   EVAL      vinmsg = %trimr(vinmsg)
049800091228     C                             + ' ' + 'VABPKB'
049900091228     C                   ENDIF
050000091228     C* ......VABVLB
050100091228     C                   IF        %subst(vindta:31:5) <> *zeros AND
050200091228     C                             %subst(vindta:31:5) <> *blanks
050300091228     C                   EVAL      PiStr=%trim(%subst(vindta:31:5))
050400091228     C                   EXSR      CHKNUM
050500091228     C                   IF        PiNum=*on
050600091228     C                   EVAL      PiVal = PiVal / 1000                         * da dm3 a m3
050700091228     C                   Z-ADD(H)  PiVal         VABVLB
050800091228     C                   ELSE
050900091228     C                   ADD       1             errore
051000091228     C                   EVAL      vinmsg = %trimr(vinmsg)
051100091228     C                             + ' ' + 'VABVLB'
051200091228     C                   ENDIF
051300091228     C                   ENDIF
051400091228     C* ......VABTC1/TC2
051500091228     C                   IF        %subst(vindta:47:2) <> *blanks
051600091228     C                   EVAL      wTC=%trim(%subst(vindta:47:2))
051700091228     C                   EVAL      wText=%trim(%subst(vindta:49:30))
051800091228     C                   EXSR      GESTEXT
051900091228     C                   ENDIF
052000091228     C*
052100091228     C                   IF        %subst(vindta:79:2) <> *blanks
052200091228     C                   EVAL      wTC=%trim(%subst(vindta:79:2))
052300091228     C                   EVAL      wText=%trim(%subst(vindta:81:30))
052400091228     C                   EXSR      GESTEXT
052500091228     C                   ENDIF
052600091228     C*
052700091228     C                   ENDIF
052800091228     C*
052900091228     C***
053000091228     C* ...tipo record 'T' (consegne particolari / istruzioni particolari)
053100091228     C                   IF        %subst(vindta:1:1) = 'T'
053200091228     C*
053300091228     C* Resetto indicatore d anomalia sul singolo record
053400091228     C                   eval      vinflg = '1'
053500091228     C*
053600091228     C                   IF        %subst(vindta:5:2) <> *blanks
053700091228     C                   EVAL      wTC=%trim(%subst(vindta:5:2))
053800091228     C                   EVAL      wText=%trim(%subst(vindta:7:30))
053900091228     C                   EXSR      GESTEXT
054000091228     C                   ENDIF
054100091228     C*
054200091228     C                   IF        %subst(vindta:37:2) <> *blanks
054300091228     C                   EVAL      wTC=%trim(%subst(vindta:37:2))
054400091228     C                   EVAL      wText=%trim(%subst(vindta:39:30))
054500091228     C                   EXSR      GESTEXT
054600091228     C                   ENDIF
054700091228     C*
054800091228     C                   IF        %subst(vindta:69:2) <> *blanks
054900091228     C                   EVAL      wTC=%trim(%subst(vindta:69:2))
055000091228     C                   EVAL      wText=%trim(%subst(vindta:71:30))
055100091228     C                   EXSR      GESTEXT
055200091228     C                   ENDIF
055300091228     C*
055400091228     C                   ENDIF
055500091228     C*
055600091228     C***
055700091228     C* ...tipo record 'J' (note aggiuntive)
055800091228     C                   IF        %subst(vindta:1:1) = 'J'
055900091228     C*
056000091228     C* Resetto indicatore d anomalia sul singolo record
056100091228     C                   eval      vinflg = '1'
056200091228     C*
056300091228     C                   IF        %subst(vindta:5:62) <> *blanks
056400091228     C                   EVAL      wTC=*blanks
056500091228     C                   EVAL      wText=%trim(%subst(vindta:5:62))
056600091228     C                   EXSR      GESTEXT
056700091228     C                   ENDIF
056800091228     C*
056900091228     C                   IF        %subst(vindta:67:62) <> *blanks
057000091228     C                   EVAL      wTC=*blanks
057100091228     C                   EVAL      wText=%trim(%subst(vindta:67:62))
057200091228     C                   EXSR      GESTEXT
057300091228     C                   ENDIF
057400091228     C*
057500091228     C                   ENDIF
057600091229     C*
057700091229     C***
057800091229     C* ...tipo record 'K' (importi vari)
057900091229     C                   IF        %subst(vindta:1:1) = 'K'
058000091229     C*
058100091229     C* Resetto indicatore d anomalia sul singolo record
058200091229     C                   eval      vinflg = '1'
058300091229     C* ......VABCAS
058400091229     C                   IF        %subst(vindta:23:9) <> *blanks AND
058500091229     C                             %subst(vindta:23:9) <> *zeros
058600091229     C                   EVAL      PiStr=%trim(%subst(vindta:23:9))
058700091229     C                   EXSR      CHKNUM
058800091229     C                   IF        PiNum=*on
058900091229     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
059000091229     C                   Z-ADD(H)  PiVal         VABCAS
059100091229     C                   ELSE
059200091229     C                   ADD       1             errore
059300091229     C                   EVAL      vinmsg = %trimr(vinmsg)
059400091229     C                             + ' ' + 'VABCAS'
059500091229     C                   ENDIF
059600091229     C                   ENDIF
059700091229     C* ......VABVCA
059800091229     C                   EVAL      VABVCA=%trim(%subst(vindta:118:3))
059900091229     C*
060000091229     C                   ENDIF
060100091229     C*
060200010202     C*
060300000801     C* Ebbene...
060400000801     C                   ADD       1             §CTRMO
060500010201     C                   IF        errore <> *zeros
060600000801     C                   ADD       1             §CTRNO
060700000801     C                   EVAL      recko = vindta
060800000801     C                   EXCEPT    rigadett
060900000801     C                   EVAL      vinflg = '2'
061000000801     C                   ELSE
061100010201     C                   ADD       1             §CTROKVB
061200000801     C                   ENDIF
061300000801     C*
061400000801     C                   ENDSR
061500091228     C*----------------------------------------------------*
061600091228     C*  GESTIONE PARTICOLARITA "TESTUALI"
061700091228     C*----------------------------------------------------*
061800091228     C     GESTEXT       BEGSR
061900091228     C*
062000091228     C                   SELECT
062100091228     C*
062200091228     C                   WHEN      wTC = *blanks
062300091228     C                   EVAL      wNote = %trim(wNote) + ' ' +
062400091228     C                                     %trim(wText)
062500091228     C*
062600091228     C                   WHEN      wTC = '01'
062700091228     C                   IF        VABTC1 = *blanks
062800091228     C                   EVAL      VABTC1 = 'A'
062900091228     C                   ELSE
063000091228     C                   IF        VABTC2 = *blanks
063100091228     C                   EVAL      VABTC2 = 'A'
063200091228     C                   ENDIF
063300091228     C                   ENDIF
063400091228     C*
063500091228     C                   EVAL      VATTRC = 'B'
063600091228     C                   EVAL      VATNOT = %trim(wText)
063700091228     C                   WRITE     FIVAT000
063800091228     C                   ADD       1             §CTROKVT
063900091228     C*
064000091228     C                   WHEN      wTC = '14'
064100091228     C                   EVAL      wDCR = %subst(wText:5:4)+
064200091228     C                                    %subst(wText:3:2)+
064300091228     C                                    %subst(wText:1:2)
064400091228     C                   EVAL      wHCR = %subst(wText:10:4)
064500091228     C                   EVAL      VABTCR = 'P'
064600091228     C*
064700091228     C                   WHEN      wTC = '15'
064800091228     C                   EVAL      wDCR = %subst(wText:5:4)+
064900091228     C                                    %subst(wText:3:2)+
065000091228     C                                    %subst(wText:1:2)
065100091228     C                   EVAL      wHCR = %subst(wText:10:4)
065200091228     C                   EVAL      VABTCR = *blanks
065300091228     C*
065400091228     C                   WHEN      wTC = '17'
065500091228     C                   EVAL      VABTSP = 'H'
065600091228     C*
065700091228     C                   WHEN      wTC = '18'
065800091228     C                   EVAL      VABTSP = 'E'
065900091228     C*
066000091228     C                   WHEN      wTC = '21'
066100091228     C                   EVAL      wNote = %trim(wNote) + ' ' +
066200091228     C                                     'Spedizione a temperatura ' +
066300091228     C                                     'controllata.'
066400091228     C*
066500091228     C                   WHEN      wTC = '52'
066600091228     C                   IF        VABTC1 = *blanks
066700091228     C                   EVAL      VABTC1 = 'A'
066800091228     C                   ELSE
066900091228     C                   IF        VABTC2 = *blanks
067000091228     C                   EVAL      VABTC2 = 'A'
067100091228     C                   ENDIF
067200091228     C                   ENDIF
067300091228     C*
067400091228     C                   EVAL      VATTRC = 'B'
067500091228     C                   EVAL      VATNOT = %trim(wText)
067600091228     C                   WRITE     FIVAT000
067700091228     C                   ADD       1             §CTROKVT
067800091228     C*
067900091228     C                   ENDSL
068000091228     C*
068100091228     C                   MOVE(P)   wDCR          VABDCR
068200091228     C                   MOVE(P)   wHCR          VABHCR
068300091228     C*
068400091228     C                   ENDSR
068500010202     C*----------------------------------------------------*
068600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
068700010202     C*----------------------------------------------------*
068800020305     C     PREVAT        BEGSR
068900010202     C*
069000021113     C* Compongo il nome del membro da dare al FIVATWWR
069100010202     C                   eval      parmbr = vlrhdl
069200010202     C                   movel     'M'           parmbr
069300060113     C                   eval      parccm = vlrksc
069400010202     C                   eval      paropz = '1'
069500010202     C* Effettuo la chiamata al CLLE preposto
069600040506     C                   call(e)   'TITVVTC'
069700010202     C                   parm                    parccm
069800010202     C                   parm                    parmbr
069900010202     C                   parm                    paropz
070000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070100010202     C                   if        %error
070200010202     C                   movel     '1'           chkcall
070300010202     C                   else
070400010202     C                   movel     '0'           chkcall
070500010202     C                   endif
070600010202     C*
070700010202     C                   ENDSR
070800000801     C*----------------------------------------------------*
070900000801     C*  CONTROLLO NUMERICITA' CAMPI
071000000801     C*----------------------------------------------------*
071100000801     C     CHKNUM        BEGSR
071200000801     C*
071300000801     C                   call(e)   'ISNUMERIC'
071400000801     C                   PARM                    PiStr            30
071500040714     C                   PARM      ','           PiDecChr          1
071600000801     C                   PARM      *ZEROS        PiVal            30 9
071700000801     C                   PARM      '0'           PiInt             1
071800000801     C                   PARM      '0'           PiNum             1
071900000801     C                   IF        %error
072000000801     C                   EVAL      PiInt=*off
072100000801     C                   ENDIF
072200000801     C*
072300000801     C                   ENDSR
072400000801     C***
072500000801
072600091228     C*----------------------------------------------------*
072700091228     C*  SCARICA BUFFER TESTATA BOLLE (FIVAB)
072800091228     C*----------------------------------------------------*
072900091228     C     WRIVAB        BEGSR
073000091228     C*
073100091228     C                   EVAL      VABNOT = %trim(%subst(wNote:1:35))
073200091228     C                   EVAL      VABNT2 = %trim(%subst(wNote:36:35))
073300091228     C*
073400091228     C* Considerazioni sul contenuto di campi precedentemente valorizzati
073500091228     C                   IF        FlgCAS <> '0'
073600091228     C                   IF        VABCBO = '1'
073700091228     C                   EVAL      VABCBO = '4'
073800091228     C                   ELSE
073900091228     C                   EVAL      VABCBO = '6'
074000091228     C                   ENDIF
074100091228     C                   ENDIF
074200091228     C*
074300091228     C* Eseguo routine finale x considerazioni specifiche su importi/divise
074400091228     C                   EXSR      CHKIMPDIV
074500091228     C*
074600091228     C                   WRITE     FIVAB000
074700091228     C*
074800091228     C                   ENDSR
074900091228     C***
075000011113
075100011113     C*----------------------------------------------------*
075200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
075300011113     C*----------------------------------------------------*
075400011113     C     CHKIMPDIV     BEGSR
075500011113     C*
075600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
075700011113     C                   Z-ADD     *zeros        wrkDec            9 9
075800011113     C*
075900011113     C* Come prima cosa effettuo considerazioni sulla divisa
076000011113     C                   IF        vabIAS > *zeros
076100011113     C                   IF        vabVAS <> 'EUR'
076200011113     C                   EVAL      vabVAS =  'ITL'
076300011113     C                   ENDIF
076400011113     C                   ENDIF
076500011113     C*
076600011113     C                   IF        vabCAS > *zeros
076700011113     C                   IF        vabVCA <> 'EUR'
076800011113     C                   EVAL      vabVCA =  'ITL'
076900011113     C                   ENDIF
077000011113     C                   ENDIF
077100011113     C*
077200011113     C                   IF        vabVMD > *zeros
077300020305     C                   IF        vabVAD <> 'EUR'
077400011113     C                   EVAL      vabVAD =  'ITL'
077500011113     C                   ENDIF
077600011113     C                   ENDIF
077700011113     C*
077800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
077900011113     C                   Z-ADD     vabIAS        wrkDec
078000011113     C                   IF        wrkDec > *zeros
078100011113     C                   IF        vabVAS = 'ITL'
078200011113     C                   EVAL      vabIAS = *zeros
078300011113     C                   ENDIF
078400011113     C                   ENDIF
078500011113     C*
078600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
078700011113     C                   Z-ADD     vabCAS        wrkDec
078800011113     C                   IF        wrkDec > *zeros
078900011113     C                   IF        vabVCA = 'ITL'
079000011113     C                   EVAL      vabCAS = *zeros
079100011113     C                   ENDIF
079200011113     C                   ENDIF
079300011113     C*
079400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
079500011113     C                   Z-ADD     vabVMD        wrkDec
079600011113     C                   IF        wrkDec > *zeros
079700011113     C                   IF        vabVAD = 'ITL'
079800011113     C                   EVAL      vabVMD = *zeros
079900011113     C                   ENDIF
080000011113     C                   ENDIF
080100011113     C*
080200011113     C                   ENDSR
080300011113     C***
080400011113
080500011113
080600000801
080700000801
080800990920      /TITLE Invio dei dati al punto operativo.
080900010202     C     invio         BEGSR
081000990920     C*
081100021113     C* 1° invio FIVAT
081200010201     C                   reset                   dscmz
081300010201     C                   move      vlrpoi        cmzdst
081400021113     C                   eval      cmzfld = 'FIVATWWR'
081500010201     C                   eval      cmzmbd = vlrhdl
081600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081700021009     C***                if        prmfir = *blanks
081800021113     C                   eval      cmzfla = 'FIVAT00F'
081900021113     C                   eval      cmzmba = 'FIVAT00F'
082000021009     C***                else
082100021009     C***                eval      cmzfla = prmfir
082200021009     C***                eval      cmzmba = prmfir
082300021009     C***                endif
082400010201     C                   eval      cmznrr = *zeros
082500020305     C                   move      §ctrokvt      cmznrr
082600021018     C                   eval      cmzlba = vlrfl1
082700010201     C                   call(e)   'TIS711C'
082800010201     C                   parm                    dscmz
082900010201     C                   parm      *blanks       esito
083000010205     C                   if        %error
083100010205     C                             or cmzerr = '1'
083200010205     C                             or esito  = '1'
083300010205     C                   eval      wrkesito = '3'
083400010205     C                   else
083500010201     C*
083600021113     C* 2° invio FIVAB
083700010201     C                   reset                   dscmz
083800010201     C                   move      vlrpoi        cmzdst
083900010201     C                   eval      cmzfld = vlrfou
084000010201     C                   eval      cmzmbd = vlrhdl
084100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
084200021009     C***                if        prmfir = *blanks
084300021113     C                   eval      cmzfla = 'FIVAB00F'
084400021113     C                   eval      cmzmba = 'FIVAB00F'
084500021009     C***                else
084600021009     C***                eval      cmzfla = prmfir
084700021009     C***                eval      cmzmba = prmfir
084800021009     C***                endif
084900010201     C                   eval      cmznrr = *zeros
085000010201     C                   move      §ctrokvb      cmznrr
085100021018     C                   eval      cmzlba = vlrfl1
085200010201     C                   call(e)   'TIS711C'
085300010201     C                   parm                    dscmz
085400010201     C                   parm      *blanks       esito
085500010201     C                   if        %error
085600010201     C                             or cmzerr = '1'
085700010201     C                             or esito  = '1'
085800010201     C                   eval      wrkesito = '3'
085900010201     C                   endif
086000010205     C                   endif
086100990920     C*
086200000613     C                   ENDSR
086300000613     C***
086400070411
086500070411     C     *pssr         BEGSR
086600070411     C*
086700070411     C                   if        %open(tivin00r)
086800070411     C                   close     tivin00r
086900070411     C                   endif
087000070411     C                   if        %open(fivabwwr)
087100070411     C                   close     fivabwwr
087200070411     C                   endif
087300070411     C                   if        %open(fivatwwr)
087400070411     C                   close     fivatwwr
087500070411     C                   endif
087600070411     C*
087700070411     C* Effettuo la chiamata al CLLE preposto
087800070411     C                   call(e)   'TITVVTC'
087900070411     C                   parm                    parccm
088000070411     C                   parm                    parmbr
088100070411     C                   parm      '2'           paropz
088200070411     C*
088300070411     C                   eval      wrkesito = '2'
088400070411     C*
088500070411     C                   seton                                        LR
088600070411     C*
088700070411     C                   ENDSR     '*CANCL'
088800070411     C***
088900070411
089000990910
089100000613     C     *inzsr        BEGSR
089200990910     C*
089300990910     C     *entry        plist
089400990920     C                   parm                    tivlrds
089500990921     C                   parm      wrkesito      esito
089600000724     C                   parm                    prmlit
089700000710     C                   parm                    prmfir
089800000613     C*
089900000830     C* CALCOLA LA DATA CORRENTE
090000091228     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
090100091228     C                   eval      datcor = %dec(%date() : *ISO)
090200000830     C*
090300000613     C                   ENDSR
090400000613     C***
090500990908
090600091228     Otitv1u1p  E            riepilogo         2
090700990915     O                                              'Upload via Internet'
090800990915     O                                           +1 'Traduzione TIVIN00R -'
090900021113     O                                           +1 'FIVABWWR/FIVATWWR'
091000010201     O                                           +1 'Report testate bolle'
091100990915     O          E            riepilogo   2
091200990915     O                       wrkdata
091300990915     O                       wrkora              +1
091400990915     O                       procname            +1
091500990915     O          E            riepilogo   2
091600990915     O                                              'Cliente..................:'
091700990915     O                       VABCCM        z     +1
091800990915     O          E            riepilogo   2
091900990920     O                                              'Riferimento Strategi.....:'
092000990920     O                       vlrhdl              +1
092100990915     O          E            riepilogo   2
092200990915     O                                              'Giusti...................:'
092300010201     O                       §CTROKVB      2   +  1
092400990915     O          E            riepilogo   2
092500990915     O                                              'Sbagliati e corretti.....:'
092600971022     O                       §CTRMO        2   +  1
092700990915     O          E            riepilogo   2
092800990915     O                                              'Sbagliati e scartati.....:'
092900971022     O                       §CTRNO        2   +  1
093000000613
093100091228     Otitv1u1ps E            testdett          2
093200000613     O                                              'Upload via Internet'
093300000613     O                                           +1 'Traduzione TIVIN00R -'
093400021113     O                                           +1 'FIVABWWR/FIVATWWR'
093500010201     O                                           +1 'Report testate bolle'
093600000616     O          E            testdett    3
093700000613     O                                           +2 'N° rec'
093800000613     O                                           +3 'Anteprima contenuto'
093900000616     O          E            rigadett    2
094000000613     O                       rrnum               +2
094100000621     O                       recko               +3
094200000616     O          E            findett     2
094300000613     O                       wrkdata
094400000613     O                       wrkora              +1
094500000613     O                       procname            +1
094600000616     O          E            findett     2
094700000613     O                                              'Cliente..................:'
094800000613     O                       VABCCM        z     +1
094900000616     O          E            findett     2
095000000613     O                                              'Riferimento Strategi.....:'
095100000613     O                       vlrhdl              +1
095200000616     O          E            findett     2
095300000613     O                                              'Giusti...................:'
095400010201     O                       §CTROKVB      2   +  1
095500000616     O          E            findett     2
095600000613     O                                              'Sbagliati e corretti.....:'
095700000613     O                       §CTRMO        2   +  1
095800000616     O          E            findett     2
095900000613     O                                              'Sbagliati e scartati.....:'
096000000613     O                       §CTRNO        2   +  1
096100000512** CMD - COMANDI CL
096200091228OVRPRTF FILE(TITV1U1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
096300091228OVRPRTF FILE(TITV1U1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
096400091228DLTOVR FILE(TITV1U1P TITV1U1PS) LVL(*)
096500000512
096600000512
