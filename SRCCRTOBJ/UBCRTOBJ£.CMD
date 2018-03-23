000100091022/*PRM pgm(*LIBL/UBCRTOBJC)                                           */
000200090925/*END                                                                */
000300090925             cmd        prompt('Compilatore SETRAS')
000400090925
000500091022             parm       kwd(SRCFILE) type(TYP1) +
000600091022                          min(1)     prompt('File origine')
000700091022             parm       kwd(SRCMBR)  type(*name) len(10) +
000800091022                          min(1)     prompt('Membro origine')
000900091022             parm       kwd(SRCTYPE) type(*name) len(10)  rstd(*yes) +
001000091021                          values(RPGLE SQLRPGLE CLLE +
001100091021                                 RPG   SQLRPG   CLP +
001200100115                                 BND +
001300091021                                 DSPF  PRTF +
001400091021                                 PF    LF +
001500091022                                 CMD) +
001600091022                          min(1)     prompt('Tipo membro')
001700091022             parm       kwd(OBJLIB)  type(*name) len(10) +
001800091022                          min(1)     prompt('Libreria oggetto')
001900091022
002000091022             parm       kwd(BATCH)   type(*char) len(5)   rstd(*yes) +
002100091022                          dft(*no)   values(*NO *YES) +
002200091022                          prompt('Esecuzione in batch')
002300091022
002400091026             parm       kwd(JOBD)    type(TYP2)  +
002500091026                          pmtctl(PJOBD) +
002600091022                          PROMPT('Descrizione lavoro')
002700090925
002800091022 TYP1:       qual       type(*name)  len(10)     min(1)
002900091022             qual       type(*name)  len(10)     min(1) +
003000091022                          prompt('Libreria')
003100091022
003200091026 TYP2:       qual       type(*name)  len(10)
003300091026             qual       type(*name)  len(10) +
003400091022                          spcval((*LIBL)) prompt('Libreria')
003500091022
003600091026 PJOBD:      pmtctl     ctl(BATCH)   cond((*eq *YES)) +
003700091022                                     nbrtrue(*eq 1)
