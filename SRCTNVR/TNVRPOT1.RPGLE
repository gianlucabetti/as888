000100040909     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200991227
000300040909     fTncpo07l  if   e           k Disk
000400040909     fTncpo01l  uf a e           k Disk    rename(TNCPO000:TNCPO01)
000500040909     f                                     prefix(F)
000600040909     fTncpi07l  if   e           k Disk
000700040909     fTncpi01l  uf a e           k Disk    rename(TNCPI000:TNCPI01)
000800040909     f                                     prefix(F)
000900040909     fTncps07l  if   e           k Disk
001000040909     fTncps11l  uf a e           k Disk    rename(TNCPS000:TNCPS11)
001100040909     f                                     prefix(F)
001200040909     fTfntc02l  if   e           k Disk
001300040909     fTfntc01l  uf a e           k Disk    rename(TFNTC:TFNTC01)
001400040909     f                                     prefix(F)
001500040909
001600040909     d wapl            s                   like(NtcApl)
001700040909     d wnk1            s                   like(NtcNk1)
001800040909     d wnk2            s                   like(NtcNk2)
001900040909     d wtnt            s                   like(NtcTnt)
002000000725
002100040909     d ds_cpo        e ds                  extname(TNCPO00F)
002200040909     d ds_cpof       e ds                  extname(TNCPO00F) prefix(F)
002300040909     d ds_cpi        e ds                  extname(TNCPI00F)
002400040909     d ds_cpif       e ds                  extname(TNCPI00F) prefix(F)
002500040909     d ds_cps        e ds                  extname(TNCPS00F)
002600040909     d ds_cpsf       e ds                  extname(TNCPS00F) prefix(F)
002700040909     d ds_ntc        e ds                  extname(TFNTC00F)
002800040909     d ds_ntcf       e ds                  extname(TFNTC00F) prefix(F)
002900000725
003000000725      *----------------------------------------------------------------*
003100040909
003200040909      * --> TNCPO
003300040909     c                   Do        *Hival
003400040909     c                   Read      Tncpo07l
003500040909      * Fine file
003600040909     c                   If        %Eof(Tncpo07l)
003700040909     c                   Leave
003800040909     c                   EndIf
003900040909
004000040909     c     CpoCpo        Chain     Tncpo01l
004100040909     c                   Eval      ds_cpof = ds_cpo
004200040909     c                   If        %Found(Tncpo01l)
004300040909     c                   Update    Tncpo01
004400040909     c                   Else
004500040909     c                   Write     Tncpo01
004600040909     c                   EndIf
004700040909
004800040909     c                   EndDo
004900040909
005000040909      * --> TNCPI
005100040909     c     'P'           Setll     Tncpi07l
005200040909     c                   Do        *Hival
005300040909     c     'P'           Reade     Tncpi07l
005400040909      * Fine file
005500040909     c                   If        %Eof(Tncpi07l)
005600040909     c                   Leave
005700040909     c                   EndIf
005800040909
005900040909     c     kCpi          Chain     Tncpi01l
006000040909     c                   If        %Found(Tncpi01L) and
006100040909     c                             CpiDim >= FCpiDim
006200040909
006300040909     c                   Eval      ds_cpif = ds_cpi
006400040909     c                   Update    Tncpi01
006500040909     c                   EndIf
006600040909     c                   If        Not %Found(Tncpi01l)
006700040909     c                   Eval      ds_cpif = ds_cpi
006800040909     c                   Write     Tncpi01
006900040909     c                   EndIf
007000040909
007100040909     c                   EndDo
007200040909
007300040909      * --> TNCPS
007400040909     c                   Do        *Hival
007500040909     c                   Read      Tncps07l
007600040909      * Fine file
007700040909     c                   If        %Eof(Tncps07l)
007800040909     c                   Leave
007900040909     c                   EndIf
008000040909
008100040909     c     kCps          Chain     Tncps11l                           30
008200040909     c                   Dow       Not *In30 and (CpsNrv <> FCpsNrv or
008300040909     c                                            CpsKsc <> FCpsKsc)
008400040909     c     kCps          Reade     Tncps11l                               30
008500040909     c                   EndDo
008600040909
008700040909     c                   Eval      ds_cpsf = ds_cps
008800040909
008900040909     c                   If        *In30
009000040909     c                   Write     Tncps11
009100040909     c                   Else
009200040909     c                   Update    Tncps11
009300040909     c                   EndIf
009400040909
009500040909     c                   EndDo
009600040909
009700040909      * --> TFNTC
009800040909     c                   Do        *Hival
009900040909     c                   Read      Tfntc02l
010000040909      * Fine file
010100040909     c                   If        %Eof(Tfntc02l)
010200040909     c                   Leave
010300040909     c                   EndIf
010400040909
010500040909      * a cambio di key note cancello le note esistenti
010600040909     c                   If        wapl <> NtcApl or wnk1 <> NtcNk1 or
010700040909     c                             wnk2 <> NtcNk2 or wtnt <> NtcTnt
010800040909     c                   ExSr      Delnot
010900040909     c                   EndIf
011000040909
011100040909      * salvo la key delle note
011200040909     c                   Eval      wapl = NtcApl
011300040909     c                   Eval      wnk1 = NtcNk1
011400040909     c                   Eval      wnk2 = NtcNk2
011500040909     c                   Eval      wtnt = NtcTnt
011600040909
011700040909     c                   Eval      ds_ntcf = ds_ntc
011800040909     c                   Write     Tncpo01
011900040909
012000040909     c                   EndDo
012100040909
012200040909     c                   Eval      *InLr = *On
012300040909      *------------------------------------------------------------------------*
012400040909      * - CANELLO LE NOTE
012500040909      *------------------------------------------------------------------------*
012600040909     c     Delnot        BegSr
012700040909
012800040909     c     kNtc          Setll     Tfntc01l
012900040909     c                   Do        *Hival
013000040909     c     kNtc          Reade     Tfntc01l
013100040909     c                   If        %Eof(Tfntc01l)
013200040909     c                   Leave
013300040909     c                   EndIf
013400040909     c                   Delete    Tfntc01
013500040909     c                   EndDo
013600040909
013700040909     c                   EndSr
013800040909      *------------------------------------------------------------------------*
013900040909      * - ROUTINE INIZIALE
014000040909      *------------------------------------------------------------------------*
014100040909     c     *Inzsr        BegSr
014200040909
014300040909     c     kCpi          klist
014400040909     c                   kfld                    CpiApl
014500040909     c                   kfld                    CpiNk1
014600040909     c                   kfld                    CpiTpr
014700040909     c                   kfld                    CpiNk2
014800040909
014900040909     c     kCps          klist
015000040909     c                   kfld                    CpsCpo
015100040909     c                   kfld                    CpsDst
015200040909     c                   kfld                    CpsDim
015300040909     c                   kfld                    CpsHim
015400040909     c                   kfld                    CpsFst
015500040909
015600040909     c     kNtc          klist
015700040909     c                   kfld                    NtcApl
015800040909     c                   kfld                    NtcNk1
015900040909     c                   kfld                    NtcNk2
016000040909     c                   kfld                    NtcTnt
016100040909
016200040909     c                   EndSr
