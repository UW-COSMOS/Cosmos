import React from 'react';
import TextField from '@material-ui/core/TextField';
import SearchIcon from '@material-ui/icons/Search';
//import MenuIcon from '@material-ui/icons/Menu';
import InputAdornment from '@material-ui/core/InputAdornment';

export default function SearchBar(){
  return (
    <TextField
      id="outlined-full-width"
      label="Search"
      style={{ margin: 20 }}
      fullWidth
      placeholder=""
      helperText="Ask a question"
      margin="normal"
      variant="outlined"
      InputProps={{
        startAdornment: (
          <InputAdornment position="start">
            <SearchIcon />
          </InputAdornment>
        ),
      }}
      InputLabelProps={{
      shrink: true,
    }}/>)
}
