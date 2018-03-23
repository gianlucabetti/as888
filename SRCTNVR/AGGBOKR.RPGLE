000100990914     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990914     H DECEDIT('0,') DATEDIT(*DMY.)
000300990914     FYhBOK00F  UP   E           K DISK
000400990914     FYhBOS02L  IF   E           K DISK
000500990914     FQsysprt   O    F  132        printer oflind(*inof)
000600990914     C*--------------------------------------------------------------------*
000700990914     C*  Ciclo Principale
000800990914     C*--------------------------------------------------------------------*
000900990914     C* ricevo flag x verificare se gestire stampa o modifica
001000990914     c     *ENTRY        PLIST
001100990914     c                   parm                    waggio            1
001200990914     C* Definisco la ciave di accesso al bos
001300990914     C     KBOS          KLIST
001400990914     C                   Kfld                    Kela
001500990914     C                   Kfld                    Ksoc
001600990914     C                   Kfld                    Kkcc
001700990914     C                   Kfld                    Kddt
001800990914     C                   Kfld                    Kndt
001900990914     C                   Kfld                    Kdvc
002000990914     C                   Kfld                    Knbn
002100990914     C*  Definisco variabili
002200990914     C     *Like         define    bonela        Kela
002300990914     C     *Like         define    bonsocieta    Ksoc
002400990914     C     *Like         define    bonkcc        Kkcc
002500990914     C     *like         define    bonddt        Kddt
002600990914     C     *Like         define    bonndt        Kndt
002700990914     C     *Like         define    bondvc        Kdvc
002800990914     C     *Like         define    bonnbn        Knbn
002900990914     C* Considero Bok solo x conti fornitori senza il segno
003000990914     C                   if        bokkcc='000400' and bokope=0
003100990914     C* Aggancio Bok da Bos
003200990914     C                   move      bokela        Kela
003300990914     C                   move      boksocieta    Ksoc
003400990914     C                   move      bokkcc        Kkcc
003500990914     C                   move      bokddt        Kddt
003600990914     C                   move      bokndt        Kndt
003700990914     C                   move      bokdvc        Kdvc
003800990914     C                   move      boknbn        Knbn
003900990914     C                   movel     'N'           wtrova
004000990914     C     Kbos          chain     yhbos02l                           21
004100990914     C* Loop di lettura yhbos fino a che non trovo record corrisp.
004200990914     C* da aggiornare
004300990914     C                   Dow       not *in21 and wtrova='N'
004400990914     C* Controllo se bos corretto
004500990914     C                   exsr      chkbos
004600990914     C* Leggo successivo se non trovato
004700990914     C                   if        wtrova='N'
004800990914     C     Kbos          reade     yhbos02l                               21
004900990914     C                   end
005000990914     C*
005100990914     C                   end
005200990914     C*
005300990914     C* Se ho trovato il bos da aggiornare eseguo aggiornamento
005400990914     C                   if        wtrova='S'
005500990914     C* stampa sempre
005600990914     C                   eval      bokope=bonope
005700990914     C                   eval      bokda=bonda
005800990914     C   of              except    testa
005900990914     C                   except    detta
006000990914     C* aggiorna solo se flag lo prevede
006100990914     C                   if        waggio='S'
006200990914     C                   except    aggope
006300990914     C                   end
006400990914     C* .... altrimenti avvisa se non trovato
006500990914     C                   else
006600990914     C   of              except    testa
006700990914     C                   except    notrv
006800990914     C                   end
006900990914     C*
007000990914     C                   end
007100990914     C*--------------------------------------------------------------------*
007200990914     C* Trovato
007300990914     C*--------------------------------------------------------------------*
007400990914     C     chkbos        begsr
007500990914     C*
007600990914     C                   movel     'N'           Wtrova            1
007700990914     C                   if        bokcdi=Boncdi and bonserpar=bokserpar and
007800990914     C                             bokdrp=Bondrp and bonnrp=boknrp and
007900990914     C                             bokddc=Bonddc and bonndc=bokndc and
008000990914     C                             bokdrg=Bondrg and bonnrg=boknrg and
008100990914     C                             bonkcb=bokkcb and bonksb=bokksb and
008200990914     C                             bondsc=bokdsc and bonimp=bokimp
008300990914     C                   movel     'S'           Wtrova
008400990914     C                   end
008500990914     C*
008600990914     C                   endsr
008700990914     o*--------------------------------------------------------------*
008800990914     o* Stampa
008900990914     o*-------------------------------------------------------------*
009000990914     OQsysPrt   E            Testa            01
009100990914     o                                           66 'Elenco dati bonfici da agg'
009200990914     o                                              'iornare con operando/segno'
009300990914     O          E            Testa          1 03
009400990914     o                                           14 'Cod.fornitore'
009500990914     o                                           36 'Data e numero partita'
009600990914     o                                           47 'scadenza'
009700990914     o                                           68 'Data e numero docum.'
009800990914     o                                           91 'importo'
009900990914     O          E            detta          1
010000990914     O                       bokkcc               6
010100990914     O                       bokcdi              15
010200990914     O                       bokdrp              26
010300990914     O                       boknrp        4     36
010400990914     O                       bokdsc              47
010500990914     O                       bokddc              58
010600990914     O                       bokndc        4     68
010700990914     O                       bokimp        2     95
010800990914     O                       bokope              98
010900990914     O                       bokda              100
011000990914     O                       boksocieta         104
011100990914     O          E            notrv          1
011200990914     O                       bokkcc               6
011300990914     O                       bokcdi              15
011400990914     O                       bokdrp              26
011500990914     O                       boknrp        4     36
011600990914     O                       bokdsc              47
011700990914     O                       bokddc              58
011800990914     O                       bokndc        4     68
011900990914     O                       bokimp        2     95
012000990914     O                       bokope        k     98
012100990914     O                       bokda              100
012200990914     O                       boksocieta         104
012300990914     O                                          132 'Non trovato record in Bos'
012400990914     O*-----------------------------------------------------*
012500990914     OYhbok000  E            AGGope
012600990914     O                       bokope
012700990914     O                       bokda
