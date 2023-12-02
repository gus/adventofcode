use std::io;

fn main() -> io::Result<()> {
    let lines = io::stdin().lines();
    let mut p1sum: u32 = 0;
    let mut p2sum: u32 = 0;

    let p1finds = [0..9];
    let p2finds = [
        ("zero", 0 as u32),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];

    for line in lines {
        let s = line.unwrap();

        let (p1l, _) = p1finds.iter().fold((None as Option<u32>, None as Option<u32>), |acc, p| {
            let (_, mpos) = acc;
            let (cp, cval) = p;
            let res = s.find(cp);
            println!("P2 {}@{}: {}", cp, cval, res.or(Some(1111)).unwrap());
            match res {
                Some(t) if t < mpos => (*cval, t),
                _ => acc,
            }
        });


        let lpos = s.find(char::is_numeric).unwrap();
        let ldig = s
            .chars()
            .nth(lpos)
            .unwrap()
            .to_digit(10)
            .unwrap()
            * 10;
        let rpos = s.rfind(char::is_numeric);
        let rdig = s
            .chars()
            .nth(rpos)
            .unwrap()
            .to_digit(10)
            .unwrap();

        p1sum += ldig + rdig;

        let (p2l, _) = p2finds.iter().fold((ldig, lpos), |acc, p| {
            let (_, mpos) = acc;
            let (cp, cval) = p;
            let res = s.find(cp);
            println!("P2 {}@{}: {}", cp, cval, res.or(Some(1111)).unwrap());
            match res {
                Some(t) if t < mpos => (*cval, t),
                _ => acc,
            }
        });

        let (p2r, _) = p2finds.iter().fold((rdig, rpos), |acc, p| {
            let (_, mpos) = acc;
            let (cp, cval) = p;
            let res = s.rfind(cp);
            println!("P2 {}@{}: {}", cp, cval, res.or(Some(1111)).unwrap());
            match res {
                Some(t) if t > mpos => (*cval, t),
                _ => acc,
            }
        });
        p2sum += p2l+p2r;
    }

    println!("P1: {}", p1sum);
    println!("P2: {}", p2sum);
    Ok(())
}

// https://adventofcode.com/2023/day/1
// example: 142
// real:
