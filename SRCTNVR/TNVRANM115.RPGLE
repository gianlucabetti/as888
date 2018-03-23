000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300120111     FFNARB01L  IF   E           K DISK
000400120111     FFnanm04L  uF   E           K DISK
000402120111     FFNdcd02L  IF   E           K DISK
000403120111     FFNdct01L  IF   E           K DISK
000404120111     FFNdcf01L  IF   E           K DISK
000500000601     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700000602      *--------------------------------------------------
000701120111     c     *entry        plist
000702120111     C                   PARM                    waggio            1
000703120111     c     karb          klist
000704120111     c                   kfld                    anmaas
000705120111     c                   kfld                    anmlnp
000706120111     c                   kfld                    anmnrs
000707120111     c                   kfld                    anmnsp
000708120111     c     kdcd          klist
000709120111     c                   kfld                    anmfls
000710120111     c                   kfld                    anmlna
000711120111     c                   kfld                    anmnrs
000712120111     c                   kfld                    anmscn
000713120111     c     kdct          klist
000714120111     c                   kfld                    dcdaac
000715120111     c                   kfld                    dcdfil
000716120111     c                   kfld                    dcdnca
000718120111     C*
000719120111     c                   clear                   conta             7 0
002700021216      *
002800120111     C     115           setll     fnanm04l
002900120111     C     115           reade     fnanm04l
003000120111    1c                   DOW       not %EOF(FNAnm04l)
003001120111    2c                   if        anmdch=0
003002120111     c     karb          chain     fnarb01l
003004120111    3c                   if        %found(fnarb01l)
003005120111     c* Spedizione chiusa con consegna anomala: chiudo l'anomalia
003006120111    4c                   if        arbdcm>0 and arbcca<>*blanks
003007120111     c                   z-add     arbdcm        anmdch
003009120111     C                   MOVEL     'M'           ANMFSC
003010120111     C                   MOVEL     'AN'          ANMCCH
003011120111     c                   if        waggio='S'
003012120111     c                   update    fnanm000
003016120111     c                   endif
003017120111     c                   add       1             conta
003018120111     c                   except    riga
003019120111     c                   else
003020120111     c                   clear                   $fine             1
003021120111     c     kdcd          setll     fndcd02l
003022120111     c     kdcd          reade     fndcd02l
003023120111     c                   dow       not %eof(fndcd02l) and $fine=*blanks
003024120111     c     kdct          chain     fndct01l
003025120111     c* C.A. 01 mancanza in fase >=100 --> chiudo l'anomalia
003026120111     c                   if        %found(fndct01l)
003027120111     c                             and dcttad='01' and dctfca>=100
003028120111     c                   eval      $fine='1'
003029120111     c                   clear                   $finef            1
003030120111     c     kdct          setll     fndcf01l
003031120111     c     kdct          reade     fndcf01l
003032120111     c                   dow       not %eof(fndcf01l) and $finef=*blanks
003033120111     c                   if        dcffca=100
003034120111     c                   z-add     dcfdfc        anmdch
003035120111     C                   MOVEL     'M'           ANMFSC
003036120111     C                   MOVEL     'AN'          ANMCCH
003037120111     c                   if        waggio='S'
003038120111     c                   update    fnanm000
003039120111     c                   endif
003040120111     c                   add       1             conta
003041120111     c                   except    rigaca
003043120111     c                   eval      $fine='1'
003044120113     c                   eval      $finef='1'
003045120111     c                   endif
003046120111     c     kdct          reade     fndcf01l
003047120111     c                   enddo
003048120111     c                   endif
003049120111     c     kdcd          reade     fndcd02l
003050120111     c                   enddo
003051120111    4c                   endif
003052120111    3c                   endif
004300021213      *
004301120111    2c                   endif
004400120111     C     115           reade     fnanm04l
004500120111    1c                   ENDDO
004501120111     c                   unlock    fnanm04l
004600021213      *
004700021213     C                   eval       *inlr = *on
004800000602      *---------------------------------------------------------
005700120111     Oqsysprt   E            riga        1
005800021213     O                                           10 'SPED.'
005900120111     O                       anmaas            +  3
006000120111     O                       anmLNP            +  1
006100120111     O                       anmNRS            +  1
006200120111     O                       anmNSP        Z   +  1
006201120111     o                                         +  2 'Segnacollo'
006202120111     o                       anmfls            +  2
006203120111     o                       anmlna            +  2
006204120111     o                       anmscn            +  2
006205120111     o                       anmdch            +  2
006206120111     o                       anmfsc            +  1
006207120111     o                       anmcch            +  2
006900021213     O                       CONTA         2   +  4
007000120111     O          E            rigaca      1
007100120111     O                                           10 'SPED.'
007200120111     O                       anmaas            +  3
007300120111     O                       anmLNP            +  1
007400120111     O                       anmNRS            +  1
007500120111     O                       anmNSP        Z   +  1
007600120111     o                                         +  2 'Segnacollo'
007700120111     o                       anmfls            +  2
007800120111     o                       anmlna            +  2
007900120111     o                       anmscn            +  2
008000120111     o                       anmdch            +  2
008100120111     o                       anmfsc            +  1
008200120111     o                       anmcch            +  2
008300120111     O                       CONTA         2   +  4
008400120111     o                                         +  2 'Da C.A.'
