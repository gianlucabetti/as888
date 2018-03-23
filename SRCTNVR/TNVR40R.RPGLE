000100920317     H DATEDIT(*YMD)
000200021017     F* TNVR40R *----------------------------------------------------*
000300021017     F*           TRASCODIFICA FILE TESTI                            *
000400021017      * -------------------------------------------------------------*
000500021021     fFlTxt00f  if   e           k disk    prefix(L_)
000600021018     fFnTxt01L  o    e             disk
000700021018     fTabel00f  if   e           k disk
000800021018     fQsysprt   o    f  132        PRINTER OFLIND(*INOG)
000900021018      *
001000021018     d Ds1H          e ds
001100021022     d DfntxtH       e ds
001200021017     d Tibs34Ds      e ds
001300021017     d DDatiUte      e ds
001400021017     d AzUteDs       e ds                  extname(AZUTE00F)
001500021018      *---
001600021018      * ds controlla data (8)
001700021018      *---
001800021018     d wlbda8          DS                  INZ                                  *controlla data
001900021018     d  g08dat                        8  0
002000021018     d  g08inv                        8  0
002100021018     d  g08err                        1
002200021018     d  g08tgi                        5  0
002300021018      *
002400021017      *----------------------------------------------------------------*
002500021017      * MAIN LINE
002600021017      *----------------------------------------------------------------*
002700021017      *
002800021017      * Inizzializzo variabili di work
002900021017     c                   clear                   Tibs34ds
003000021017     c                   clear                   Azuteds
003100021017     c                   clear                   DDatiUte
003200021017      *
003300021017     c     *DTAARA       DEFINE    §azute        azuteds
003400021017     c     *DTAARA       DEFINE    §datiute      ddatiute
003500021017      *
003600021017     c                   IN(E)     *DTAARA
003700021017if   c                   IF        %Error
003800021017     c                   EVAL      I34Tla = 'L'
003900021017     c                   CALL      'TIBS34R'
004000021017     c                   PARM                    Tibs34Ds
004100021017     c                   IN        *DTAARA
004200021017     c                   ENDIF
004300021017      *
004400021022     C                   eval      *inog = *on
004500021022     c                   move      '1H'          Cod               2
004600021022     c                   z-add     1             Kut               1 0
004700021017      *----------------------------------------------------------------*
004800021017     c     *loval        setll     FlTxt00f
004900021017     c                   read      FlTxt00f                               22
005000021017      *
005100021021    1c     *in22         Doweq     *off
005200021024      *
005300021024      * NON ELABORO GLI ANNULLATI
005400021025    2C                   IF        L_TXTATB = *blanks
005500021024      *
005600021021      * Verifico che esista la 1H
005700021021     c                   movel(p)  L_TxtLet      key               8
005800021021     c     KTab          chain     tabel                              33
005900021021      *
006000021024    3c     *in33         IFEQ      *off
006100021021     c                   move      L_TxtLet      $TpLet            2
006200021017      *
006300021021      * Ciclo su stesso tipo testo
006400021024    4c     L_TxtLet      DOWEQ     $TpLet
006500021017     c     *in22         andeq     *off
006600020705      *
006700021021     c                   clear                   Ds1H
006800021021      *
006900021021     c                   EXSR      Wri_Txt
007000021017     c                   read      FlTxt00f                               22
007100021024    4c                   enddo
007200021017      *
007300021017      * Scrivo il record di tipo 'H'
007400021017     c                   EXSR      Wri_TxtH
007500021021      *
007600021024   x3c                   ELSE
007700021021      * stampo i tipi lettera non trascodificati
007800021021     c                   exsr      Ges_Stp
007900021021     c                   read      FlTxt00f                               22
008000021024    3c                   ENDIF
008100021024      *
008200021025   x3c                   ELSE
008300021025     c                   read      FlTxt00f                               22
008400021024    2c                   ENDIF
008500021024      *
008600021021    1c                   enddo
008700020705      *
008800021017     c                   SETON                                        LR
008900021017      *----------------------------------------------------------
009000021017      * Scrivo tutte le righe del file testo che rimangono uguali
009100021017      *----------------------------------------------------------
009200021017     c     Wri_Txt       BEGSR
009300021017      *
009400021017      * Valorizzo solo il P.O
009500021017     c                   move      Simfel        TxtPol
009600021018      *
009700021018      * data trasmissione
009800021022     c                   z-add     L_txtdtr      txtdtr
009900021021     c                   move      L_TxtLet      TxtLet
010000021021     c                   move      L_TxtPos      TxtPos
010100021021     c                   move      L_TxtNrr      TxtNrr
010200021021     c                   move      L_TxtRig      TxtRig
010300021021     c                   move      L_TxtLin      TxtLin
010400021021     c                   move      L_TxtFtt      TxtFtt
010500021021     c                   move      L_TxtFlt      TxtFlt
010600021021     c                   move      L_TxtFtr      TxtFtr
010700021021     c                   move      L_TxtAtb      TxtAtb
010800021021      *
010900021017     c                   write     FnTxt000
011000021017      *
011100021017     c                   ENDSR
011200021017      *----------------------------------------------------------
011300021017      * Scrivo le righe del file testi che dalla tabella 1H
011400021017      *----------------------------------------------------------
011500021017     c     Wri_TxtH      BEGSR
011600021021      *
011700021021     c                   movel     $TpLet        TxtLet
011800021017      *
011900021022     c                   clear                   DfntxtH
012000021018     c                   movel     TblUni        Ds1H
012100021018     c                   movel     §1HDes        §TxDes
012200021018     c                   movel     §1HPva        §TxPva
012300021018     c                   movel     §1HFts        §TxFts
012400021021     c                   movel     §1HFv1        §TxSip
012500021021     c                   movel     §1HFv2        §TxSic
012600021021     c                   movel     §1HFv3        §TxScc
012700021021     c                   movel     §1HFv4        §TxSca
012800021021     c                   movel     §1HFv5        §TxStc
012900021021     c                   movel     §1HFv6        §TxSdc
013000021021     c                   movel     §1HFv7        §TxSpf
013100021021     c                   movel     §1HFv8        §TxSnp
013200021021     c                   movel     §1HFv9        §TxSri
013300021017      *
013400021017      * imposto la riga lettera
013500021022     c                   movel(P)  DfntxtH       TxtRig
013600021021      *
013700021017      * Modifico alcuni campi del vecchio file
013800021017     c                   move      Simfel        TxtPol
013900021017     c                   move      'H'           TxtPos
014000021017     c                   move      *zeros        TxtNrr
014100021017     c                   move      *blanks       TxtRib
014200021017      *
014300021021     c                   write     FnTxt000
014400021017     c                   ENDSR
014500021021      *----------------------------------------------------------
014600021018      * Tabel
014700021018     C     kTab          klist
014800021018     C                   kfld                    kut               1 0
014900021018     C                   kfld                    Cod               2
015000021018     C                   kfld                    Key               8
015100021018      *----------------------------------------------------------
015200021022      * stampo i record non trascodificati
015300021018      *----------------------------------------------------------
015400021018     c     Ges_Stp       BEGSR
015500021022     c   og              EXCEPT    Testata
015600021018     c                   EXCEPT    Dettaglio
015700021018     c                   ENDSR
015800021018      *---------------------------------------------------------------*
015900021022     oQSYSPRT   E            Testata          02
016000021018     o                       RSUT                20
016100021018     o                                         +  1 'Filiale:'
016200021018     o                       SIMFEL            +  1
016300021018     o                                         +  2 'TRASCODIFICA FILE TESTI'
016400021021     o                                         +  8 'TNVR40R'
016500021018     o                       UDATE             +  2 '  /  /  '
016600021018     o                                         +  1 'PAG.'
016700021018     o                       PAGE1         Z
016800021022     o          E            Testata          03
016900021022     o                                         +  1 'Cod.'
017000021022     o                                         +  3 'Tipo'
017100021022     o                                         +  3 'Num.'
017200021022     o                                         +  2 'Testo'
017300021022     o          E            Testata        1 04
017400021022     o                                         +  0 'Testo'
017500021022     o                                         +  3 'Riga'
017600021022     o                                         +  3 'Riga'
017700021022     o                                         +  2 'Lettera'
017800021022     o          E            Dettaglio   1
017900021022     o                       L_TXTLET             4
018000021022     o                       L_TXTpos            10
018100021022     o                       L_TXTnrr      z     19
018200021022     o                       L_TXTrig         +   2
