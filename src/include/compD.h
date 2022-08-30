#ifndef COMPD_H
#define COMPD_H

void check_assump(long addr, long start, long end, long val);
long get32(long addr);
long put32(long addr, long val);
#define update_var(varname_, start_, end_, val_) \
  varname_ = var_replace(varname_, start_, end_, val_);

#endif