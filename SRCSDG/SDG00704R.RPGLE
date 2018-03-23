000100170731     H*BIND
000200170731     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300170731     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400940316      *
000500130925      *-------------------------------------------------------------------
000600170704      *
000700170704R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
000800170704      *
000900130925      *-------------------------------------------------------------------
001000130926      * Reperisce i dati anagrafici del primo record inserito a parità
001100130926      * di partita IVA /Codice fiscale
001200130926      *
001300130925      *
001400130925      *-------------------------------------------------------------------
001500091019     F*------------
001600130925     F* Spesometro - Movimenti estratti
001700170803     F* Key: Soc/TpCom/Anno/Trim/Mese/CF/PartIVA/CdFisc  FIFO
001800170704     FSDGCST05L IF   E           K DISK
001900091019     D*------------
002000091019     D* DS per il passaggio dei parametri
002100170731     D SDG00704DS    E DS
002200091019     D*-------------
002300091019     D* Passaggio Parametri
002400091019     D KPJBA         E DS
002500130925     D*-------------
002600130925     D* Campi di lavoro
002700170704     D $FineCST        S              1A
002800170704     D SavSocieta      S                   Like(CSTSocieta)
002900170704     D SavTpCom        S                   Like(CSTTpCom  )
003000170704     D SavTrim         S                   Like(CSTTrim   )
003100170704     D SavAnno         S                   Like(CSTAnno   )
003200170704     D SavMese         S                   Like(CSTMese   )
003300170803     D SavCF           S                   Like(CSTCF     )
003400170714     D SavPartIVA      S                   Like(CSTPartIVA)
003500170714     D SavCdFisc       S                   Like(CSTCdFisc )
003600130925     C*----------------------------------------------------*
003700130925     C*                MAIN LINE PROGRAM
003800130925     C*----------------------------------------------------*
003900130926     C*
004000940330     C     *ENTRY        PLIST
004100091019     C                   PARM                    KPJBA
004200170731     C                   PARM                    SDG00704DS
004300130926     C*
004400130926     C*
004500130926     C* se la partita IVA e codice fiscale sono gli stessi dell'ultima
004600130926     C* chiamata, ripasso i dati recuperati in precedenza
004700170731     C                   if        Found0704   = *On         and
004800170731     C                             Soc0704     = SavSocieta  and
004900170731     C                             TpCom0704   = SavTpCom    and
005000170731     C                             Anno0704    = SavAnno     and
005100170731     C                             Trim0704    = SavTrim     and
005200170731     C                             Mese0704    = SavMese     and
005300170803     C                             CF0704      = SavCF       and
005400170731     C                             PartIV0704  = SavPartIVA  and
005500170731     C                             CdFisc0704  = SavCdFisc
005600130926     C*
005700130926     C                   SetOn                                            RT
005800130926     C                   RETURN
005900130926     C*
006000130926     C                   else
006100130926     C*
006200170731     C                   Eval      ERR0704     = *Off
006300170731     C                   Eval      Found0704   = *Off
006400170704     C*
006500170731     C                   Eval      Intra0704   = *Off
006600170718     C*
006700170731     C                   Eval      Stato10704  = *Blank
006800170731     C                   Eval      IdPae10704  = *Blank
006900170731     C                   Eval      PIVA10704   = *Blank
007000170731     C                   Eval      CdFis10704  = *Blank
007100170731     C                   Eval      CogRa10704  = *Blank
007200170731     C                   Eval      TpN10704    = *Blank
007300170731     C                   Eval      Denom10704  = *Blank
007400170731     C                   Eval      Cogno10704  = *Blank
007500170731     C                   Eval      Nome10704   = *Blank
007600170704     C*
007700170731     C                   Eval      Indir20704  = *Blank
007800170731     C                   Eval      NrCiv20704  = *Blank
007900170731     C                   Eval      CAP20704    = *Blank
008000170731     C                   Eval      Local20704  = *Blank
008100170731     C                   Eval      Prov20704   = *Blank
008200170731     C                   Eval      Stato20704  = *Blank
008300170731     C                   Eval      IdPae20704  = *Blank
008400170704     C*
008500170731     C                   Eval      Indir30704  = *Blank
008600170731     C                   Eval      NrCiv30704  = *Blank
008700170731     C                   Eval      CAP30704    = *Blank
008800170731     C                   Eval      Local30704  = *Blank
008900170731     C                   Eval      Prov30704   = *Blank
009000170731     C                   Eval      Stato30704  = *Blank
009100170731     C                   Eval      IdPae30704  = *Blank
009200170704     C*
009300170731     C                   Eval      Stato40704  = *Blank
009400170731     C                   Eval      IdPae40704  = *Blank
009500170731     C                   Eval      PIVA40704   = *Blank
009600170731     C                   Eval      CogRa40704  = *Blank
009700170731     C                   Eval      TpN40704    = *Blank
009800170731     C                   Eval      Denom40704  = *Blank
009900170731     C                   Eval      Cogno40704  = *Blank
010000170731     C                   Eval      Nome40704   = *Blank
010100130926     C*
010200130926     C                   endif
010300091019     C*
010400170731     C                   Eval      CSTSocieta  = Soc0704
010500170731     C                   Eval      CSTTpCom    = TpCom0704
010600170731     C                   Eval      CSTAnno     = Anno0704
010700170731     C                   Eval      CSTTrim     = Trim0704
010800170731     C                   Eval      CSTMese     = Mese0704
010900170803     C                   Eval      CSTCF       = CF0704
011000170731     C                   Eval      CSTPartIVA  = PartIV0704
011100170731     C                   Eval      CSTCdFisc   = CdFisc0704
011200130925     C*
011300170803     C     K08CST05      SetLL     SDGCST05L
011400170803     C     K08CST05      READE     SDGCST05L                              21
011500170704     C                   Eval      $FineCST    = *In21
011600130925     C*
011700170704     C                   DoW       $FineCST    = *Off
011800170704     C*
011900170731     C                   Eval      Found0704   = *On
012000170704     C*
012100170731     C                   Eval      Intra0704   = CSTIntra
012200170718     C*
012300170731     C                   Eval      Stato10704  = CSTStato1
012400170731     C                   Eval      IdPae10704  = CSTIdPaes1
012500170731     C                   Eval      PIVA10704   = CSTPIVA1
012600170731     C                   Eval      CdFis10704  = CSTCdFisc1
012700170731     C                   Eval      CogRa10704  = CSTCogRag1
012800170731     C                   Eval      TpN10704    = CSTTpN1
012900170731     C                   Eval      Denom10704  = CSTDenom1
013000170731     C                   Eval      Cogno10704  = CSTCognom1
013100170731     C                   Eval      Nome10704   = CSTNome1
013200170704     C*
013300170731     C                   Eval      Indir20704  = CSTIndir2
013400170731     C                   Eval      NrCiv20704  = CSTNrCiv2
013500170731     C                   Eval      CAP20704    = CSTCAP2
013600170731     C                   Eval      Local20704  = CSTLocali2
013700170731     C                   Eval      Prov20704   = CSTProv2
013800170731     C                   Eval      Stato20704  = CSTStato2
013900170731     C                   Eval      IdPae20704  = CSTIdPaes2
014000170704     C*
014100170731     C                   Eval      Indir30704  = CSTIndir3
014200170731     C                   Eval      NrCiv30704  = CSTNrCiv3
014300170731     C                   Eval      CAP30704    = CSTCAP3
014400170731     C                   Eval      Local30704  = CSTLocali3
014500170731     C                   Eval      Prov30704   = CSTProv3
014600170731     C                   Eval      Stato30704  = CSTStato3
014700170731     C                   Eval      IdPae30704  = CSTIdPaes3
014800170704     C*
014900170731     C                   Eval      Stato40704  = CSTStato4
015000170731     C                   Eval      IdPae40704  = CSTIdPaes4
015100170731     C                   Eval      PIVA40704   = CSTPIVA4
015200170731     C                   Eval      CogRa40704  = CSTCogRag4
015300170731     C                   Eval      TpN40704    = CSTTpN4
015400170731     C                   Eval      Denom40704  = CSTDenom4
015500170731     C                   Eval      Cogno40704  = CSTCognom4
015600170731     C                   Eval      Nome40704   = CSTNome4
015700130925     C*
015800130926     C* esco dal ciclo, mi basta recuperare i dati
015900130926     C* anagrafici del primo record trovato
016000130925     C                   LEAVE
016100130925     C*
016200170803     C     K08CST05      READE     SDGCST05L                              21
016300170704     C                   Eval      $FineCST    = *In21
016400130925     C                   endDo
016500170717     C*
016600091019     C*
016700130925     C                   SetOn                                            RT
016800130925     C                   RETURN
016900091019     C*
017000170717     C************************************************************
017100170717     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
017200170717     C************************************************************
017300170717     C     DefCam        BEGSR
017400170717     C*
017500170717     C* SDGCST05L
017600170803     C     K08CST05      Klist
017700170717     C                   Kfld                    CSTSocieta
017800170717     C                   Kfld                    CSTTpCom
017900170717     C                   Kfld                    CSTAnno
018000170717     C                   Kfld                    CSTTrim
018100170717     C                   Kfld                    CSTMese
018200170803     C                   Kfld                    CSTCF
018300170717     C                   Kfld                    CSTPartIVA
018400170717     C                   Kfld                    CSTCdFisc
018500170717     C*
018600170717     C                   ENDSR
018700170717     C/EJECT
