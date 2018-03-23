000100021118      /TITLE Upload via FTP: storicizzatore
000200130114     H dftactgrp(*no) actgrp(*caller)
000300990908
000400021118     Ftivin00r  IF   E             DISK    usropn
000500021118     Ftivlr06l  UF   E           K DISK    usropn prefix(U_)
000600080411     Ftivin00s  O  A E             DISK    usropn prefix(S_)
000700990908
000800990920     D tivlrds       e ds                  extname(tivlr00f)
000900021118     D prmesito        s              1
001000021118     D prmmbr          s             10
001100021118     D prmdata         s             13
001200080526     D prmtotrcd       s              7  0
001300021118     D wrkesito        s              1    inz('0')
001400021118     D wrkdataAlfa     s              8    inz(*blanks)
001500021118     D wrkdataNum      s              8  0 inz(*zeros)
001600021118     D dsvlr3        e ds
001700990908
001800000613     C*
001900021118     C                   exsr      STORICIZZA
002000021118     C                   exsr      CHKESITO
002100000613     C*
002200000724     C
002300021118     C                   seton                                        LR
002400000613
002500000724
002600910830     C*--------------------------------------------------------
002700021118     C* STORICIZZA
002800910830     C*--------------------------------------------------------
002900021118     C     STORICIZZA    BEGSR
003000990910     C*
003100990914     C                   if        not %open(tivin00r)
003200990908     C                   open      tivin00r
003300990914     C                   endif
003400021118     C                   if        not %open(tivin00s)
003500021118     C                   open      tivin00s
003600990914     C                   endif
003700021118     C*
003800080526     C                   eval      prmtotrcd = *zeros
003900030424     C                   do        *HIVAL
004000030424     C                   read      tivin00r                               70
004100030424     C                   if        not *IN70
004200080526     C* Incremento il numero d record elaborati
004300080526     C                   eval      prmtotrcd = prmtotrcd + 1
004400021118     C* Imposto il buffer del file storico
004500080411     C                   eval      S_vinDTA = %trimr(vinDTA)
004600080411     C                   eval      S_vinFLG = vinFLG
004700080411     C                   eval      S_vinMSG = vinMSG
004800080411     C                   eval      S_vlrISV = vlrISV
004900080411     C*
005000080411     C                   eval      S_vinKEY = prmmbr
005100080411     C                   eval      S_vinDAT = wrkdataNum
005200080411     C                   eval      S_vinKSC = vlrksc
005300080411     C                   eval      S_vinTIP = vlrtip
005400021118     C                   write(e)  tivinsss
005500021118     C                   if        %error
005600021118     C                   eval      wrkesito = '2'
005700021118     C                   leave
005800021118     C                   endif
005900030424     C                   endif
006000991022     C*
006100021118     C  N70              enddo
006200021118     C*
006300021118     C                   if        %open(tivin00r)
006400021118     C                   close     tivin00r
006500021118     C                   endif
006600021118     C                   if        %open(tivin00s)
006700021118     C                   close     tivin00s
006800021118     C                   endif
006900000724     C*
007000910830     C                   ENDSR
007100000613     C***
007200021118
007300021118
007400021118     C*--------------------------------------------------------
007500021118     C* CHKESITO - Routine di verifica esito storicizzazione
007600021118     C*--------------------------------------------------------
007700021118     C     CHKESITO      BEGSR
007800021118     C*
007900021118     C* Se al termine della storicizzazione non ci sono stati errori considero riuscita la
008000021118     C* storicizzazione => imposto il buon esito a OK
008100021118     C                   if        wrkesito <> '2'
008200021118     C                   eval      wrkesito = '1'
008300021118     C* Aggiorno tutti i record di log legati al membro storicizzato indicando l'avvenuta storicizz.
008400021118     C                   if        not %open(tivlr06l)
008500021118     C                   open      tivlr06l
008600021118     C                   endif
008700021118     C*
008800021118     C     K06vlr        setll     tivlr06l
008900021118     C                   if        %found(tivlr06l)
009000021118     C     K06vlr        reade     tivlr06l
009100021118     C                   dow       not %eof
009200021118     C                   if        %subst(U_vlrhdl:2:9) = %subst(prmmbr:2:9)
009300021118     C                   movel(p)  U_vlrfl3      dsvlr3
009400021118     C                   eval      §VLR33  ='S'
009500021118     C                   movel(p)  dsvlr3        U_vlrfl3
009600021118     C                   update    tivlr000
009700021118     C                   endif
009800021118     C     K06vlr        reade     tivlr06l
009900021118     C                   enddo
010000021118     C                   endif
010100021118     C*
010200021118     C                   if        %open(tivlr06l)
010300021118     C                   close     tivlr06l
010400021118     C                   endif
010500021118     C                   endif
010600021118     C*
010700021118     C                   ENDSR
010800021118     C***
010900000725
011000000725
011100990910
011200000613     C     *inzsr        BEGSR
011300990910     C*
011400990910     C     *entry        plist
011500990920     C                   parm                    tivlrds
011600021118     C                   parm      wrkesito      prmesito
011700021118     C                   parm                    prmmbr
011800021118     C                   parm                    prmdata
011900080526     C                   parm                    prmtotrcd
012000021118     C*
012100021118     C     K06vlr        klist
012200021118     C                   kfld                    vlrprg
012300021118     C                   kfld                    vlrksc
012400021118     C                   kfld                    vlrtip
012500021118     C*
012600021118     C* Innanzitutto manipolo la data (Alfa 13 nel formato SAAMMGGHHMMSS) ricevuta in input
012700021118     C                   eval      wrkdataAlfa = '20' + %subst(prmdata:2:6)
012800021118     C                   move(p)   wrkdataAlfa   wrkdataNum
012900000613     C*
013000000613     C                   ENDSR
013100000613     C***
