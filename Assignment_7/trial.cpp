#include<bits/stdc++.h>
using namespace std;

int main(){
int t = 0;
cin>> t;
while(t--){
string a,b;
cin>>a>>b;
for (int i = 0; i < a.size(); ++i)
{
	for (int j = 0; j < b.size(); ++j)
	{
		if ((a[i]==b[j])&&(i>j))
				{
					a = a + 1;
					// b++;/
				}		
	}
}
}





	return 0;
}