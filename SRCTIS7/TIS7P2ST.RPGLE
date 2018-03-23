000100130308       // per compilare, aggiungere lib. FILTRAGRPF
000200130307     FFIVAB01L  IF   E           k DISK
000300130307     FTIVLR01L  IF   E           k DISK
000400130307     D TIVLRds       e ds                  extname(TIVLR00F)
000500130307     D FIVABds       e ds                  extname(FIVAB00F)
000600130307       // - Parametri
000700130307     d pIn_Opz         s              1a
000800130307     d pIn_CdIdAz      s              2a
000900130307     d pIn_MaskPDF     s             50a
001000130307     d pOut_Esito      s              1a
001100130307
001200130307       //open
001300130307     c                   reset                   TIVLRds
001400130307     c                   reset                   FIVABds
001500130318     c                   call      'TIS7P2SR'
001600130307     c                   parm      'O'           pIn_Opz
001700130307     c                   parm                    TIVLRds
001800130307     c                   parm                    FIVABds
001900130307     c                   parm                    pIn_CdIdAz
002000130307     c                   parm                    pIn_MaskPDF
002100130307     c                   parm                    pOut_Esito
002200130307
002300130307     c                   eval      pIN_MaskPDF = 'F_&RMN*_&CCM*.txt'
002400130307     c                   read      FIVAB000
002500130307     c                   eval      VLRKSC = '0' + %EDITC(VABCCM:'X')
002600130307     c                   read      TIVLR000
002700130318     c                   call      'TIS7P2SR'
002800130307     c                   parm      'W'           pIn_Opz
002900130307     c                   parm                    TIVLRds
003000130307     c                   parm                    FIVABds
003100130308     c                   parm      '12'          pIn_CdIdAz
003200130307     c                   parm                    pIn_MaskPDF
003300130307     c                   parm                    pOut_Esito
003400130307
003500130307       //close
003600130318     c                   call      'TIS7P2SR'
003700130307     c                   parm      'C'           pIn_Opz
003800130307     c                   parm                    TIVLRds
003900130307     c                   parm                    FIVABds
004000130307     c                   parm                    pIn_CdIdAz
004100130307     c                   parm                    pIn_MaskPDF
004200130307     c                   parm                    pOut_Esito
004300130307
004400130307     c                   seton                                        lr
